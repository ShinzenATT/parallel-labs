package lab1;

import TSim.*;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import  java.util.concurrent.Semaphore;
import static TSim.TSimInterface.*;
import static java.lang.Thread.sleep;

/**
 * Lab 1: Train control system
 */
public class Lab1 {
    /**
     * The entry point of the train simulation.
     */
    private final TSimInterface tsi;
    /**
     * The list of rail elements such as switches, semaphores and deadends.
     * @see RailSwitch
     */
    private final List<SensorAction> sensorActions = new ArrayList<>();


    /**
     * The constructor of the train control system. It initializes the TSimInterface and
     * indexes the rail elements. It also starts the threads that handle the trains.
     * @param speed1 Speed of the first train.
     * @param speed2 Speed of the second train.
     */
    public Lab1(int speed1, int speed2) {
        tsi = TSimInterface.getInstance();
        //sensorActions.setSwitch(2,3,TSimInterface.SWITCH_LEFT);


        // index the rail elements and sensors
        sensorActions.add(new RailSemaphore(
            new Sensor(14, 7),
            new Sensor(14, 8),
            new Sensor(12, 9),
            new Sensor(12, 10)
        ));
        sensorActions.add(new RailSemaphore(
            new Sensor(6, 6),
            new Sensor(8, 6),
            new Sensor(10, 7),
            new Sensor(10, 8)
        ));
        sensorActions.add(new RailSemaphore(
            new Sensor(7, 9),
            new Sensor(7, 10),
            new Sensor(6, 11),
            new Sensor(3, 13)
        ));

        sensorActions.add(new RailSwitch(17,7,
            new Sensor(14, 7, SWITCH_RIGHT),
            new Sensor(14, 8, SWITCH_LEFT),
            new Sensor(19, 9, 0)
        ));
        sensorActions.add(new RailSwitch(15,9,
            new Sensor(12, 9, SWITCH_RIGHT),
            new Sensor(12, 10, SWITCH_LEFT),
            new Sensor(19, 9, 0)
        ));
        sensorActions.add(new RailSwitch(4,9,
            new Sensor(1, 10, 0),
            new Sensor(7, 10, SWITCH_RIGHT),
            new Sensor(7, 9, SWITCH_LEFT)
        ));
        sensorActions.add(new RailSwitch(3,11,
            new Sensor(1, 10, 0),
            new Sensor(6, 11, SWITCH_LEFT),
            new Sensor(3, 13, SWITCH_RIGHT)
        ));


        sensorActions.add(new RailDeadend( false,
            new Sensor(16, 3)
        ));
        sensorActions.add(new RailDeadend( true,
            new Sensor(16, 5)
        ));
        sensorActions.add(new RailDeadend( false,
            new Sensor(16, 11)
        ));
        sensorActions.add(new RailDeadend( true,
            new Sensor(15, 13)
        ));

        // start trains
        Train t1 = new Train(1, speed1);
        t1.start();
        Train t2 = new Train(2, speed2);
        t2.start();
    }

    /**
     * The class that handles the trains and starts their own threads. It will also call a relevant
     * {@link SensorAction#act(Train)} when a sensor event occurs.
     */
    public class Train extends Thread implements Runnable {
        /** The default speed of the train. */
        int startSpeed;
        /** The id of the train. */
        final int trainId;

        /**
         * The constructor of the train. To start the train, call {@link #start()}.
         * @param trainId The id of the train.
         * @param startSpeed The default speed of the train.
         */
        Train(int trainId, int startSpeed) {

            this.startSpeed = startSpeed;
            this.trainId = trainId;
        }

        /**
         * It is called by thread.start() and watches for sensor events. When a sensor event occurs,
         * it calls {@link SensorAction#act(Train)} based on the sensor coordinates.
         * @see Runnable#run()
         */
        @Override
        public void run() {
            while (true) {
                try {

                    SensorEvent sensor = tsi.getSensor(trainId);

                    if(sensor.getStatus() == SensorEvent.ACTIVE) {
                        System.out.println(sensor);
                        Sensor pnt = new Sensor(sensor.getXpos(), sensor.getYpos());

                        var acts = sensorActions.stream().filter((a) -> a.getAssociatedSensors().contains(pnt));
                        for (Iterator<SensorAction> it = acts.iterator(); it.hasNext(); ) {
                            SensorAction act = it.next();
                            Sensor s = act.getAssociatedSensors().get(act.getAssociatedSensors().indexOf(pnt));
                            act.act(this, s.preferredSwitchState);
                        }
                    }

                } catch (CommandException | InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }

        /**
         * Sets the speed of the train to the default speed.
         * @see #setSpeed(int)
         */
        public void setSpeed() {
            setSpeed(startSpeed);
        }

        /**
         * Sets the speed of the train.
         * @param speed The speed of the train.
         * @see TSimInterface#setSpeed(int, int)
         */
        public void setSpeed(int speed) {
            try {
                tsi.setSpeed(trainId, speed);
            } catch (CommandException e) {
                e.printStackTrace();
            }
        }

        public void inverseSpeed() {
            startSpeed = -startSpeed;
            setSpeed();
        }

        /**
         * Starts the train thread and sets the speed of the train to the default speed.
         * @see Thread#start()
         */
        @Override
        public void start() {
            super.start();

            try {
                tsi.setSpeed(trainId, startSpeed);
            } catch (CommandException e) {
                e.printStackTrace();
            }
        }

        /**
         * Interrupts the train thread and sets the speed of the train to 0.
         * @see Thread#interrupt()
         */
        @Override
        public void interrupt() {
            try {
                tsi.setSpeed(trainId, 0);
            } catch (CommandException e) {
                e.printStackTrace();
            }

            super.interrupt();
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Train train = (Train) o;
            return trainId == train.trainId;
        }

        @Override
        public int hashCode() {
            return Objects.hash(trainId);
        }
    }

    /**
     * A class that handles rail switches. It changes the built-in switch state based on the train and changes
     * the train direction when the train is on the switch.
     * @see SensorAction
     * @see Semaphore
     */
    public class RailSwitch extends Semaphore implements SensorAction{
        private List<Sensor> associatedSensors;
        private int x, y;
        private Train acquiredTrain = null;
        private int switchState = 0x00;


        /**
         * The constructor of the rail switch. It sets the number of permits to 1.
         * And saves the coordinates of the sensors.
         * @param sensors the coordinates of the sensors that are connected to the switch.
         */
        public RailSwitch(int xpos, int ypos, Sensor ...sensors) {
            super(1);
            associatedSensors = List.of(sensors);
            x = xpos;
            y = ypos;
        }

        @Override
        public List<Sensor> getAssociatedSensors() {
            return associatedSensors;
        }

        /**
         * It is called when a sensor event occurs. It changes the built-in switch state based on the train and changes
         * the train direction when the train is on the switch.
         * @param train The train that caused the sensor event.
         */
        @Override
        public void act(Train train, int preferredSwitchState) {
                try {
                    if(train.equals(acquiredTrain)){
                        toggleSwitch();
                        release();
                        System.out.println("released " + train.trainId);
                        return;
                    }

                    if(!tryAcquire(train)){
                        train.setSpeed(0);
                        System.out.println("waiting " + train.trainId);
                        acquire(train);
                        train.setSpeed();
                    }

                    if(preferredSwitchState > 0 && preferredSwitchState != switchState){
                        tsi.setSwitch(x, y, preferredSwitchState);
                        System.out.println("switched " + train.trainId + " to " + preferredSwitchState);
                    }

                    System.out.println("acquired " + train.trainId);

                }catch (CommandException e){
                    e.printStackTrace();
                    release();
                }catch (InterruptedException e){
                    e.printStackTrace();
                    train.interrupt();
                }
            }

            public void toggleSwitch() throws CommandException{
                if(switchState == 0x01){
                    switchState = 0x02;
                } else {
                    switchState = 0x01;
                }

                tsi.setSwitch(x, y, switchState);
                System.out.println("switched " + switchState);
            }



        public void acquire(Train train) throws InterruptedException {
            super.acquire();
            acquiredTrain = train;
        }

        public boolean tryAcquire(Train train) {
            if(super.tryAcquire()) {
                acquiredTrain = train;
                return true;
            }
            return false;
        }

        @Override
        public void release() {
            acquiredTrain = null;
            super.release();
        }
    }

    public class RailSemaphore extends Semaphore implements SensorAction {
        private List<Sensor> associatedSensors;
        private Train acquiredTrain = null;

        /**
         * The constructor of the rail switch. It sets the number of permits to 1.
         * And saves the coordinates of the sensors.
         * @param sensors the coordinates of the sensors that are connected to the switch.
         */
        public RailSemaphore(Sensor... sensors) {
            super(1);
            associatedSensors = List.of(sensors);
        }

        @Override
        public List<Sensor> getAssociatedSensors() {
            return associatedSensors;
        }

        @Override
        public void act(Train train, int preferredState) {
            try {
                if(train.equals(acquiredTrain)){
                    release();
                    System.out.println("released semaphore " + train.trainId);
                    return;
                }

                if(!tryAcquire(train)){
                    train.setSpeed(0);
                    System.out.println("waiting semaphore " + train.trainId);
                    acquire(train);
                    train.setSpeed();
                }

                System.out.println("acquired semaphore " + train.trainId);


            }/*catch (CommandException e){
                e.printStackTrace();
                release();
            }*/catch (InterruptedException e){
                e.printStackTrace();
                train.interrupt();
            }
        }

        public void acquire(Train train) throws InterruptedException {
            super.acquire();
            acquiredTrain = train;
        }

        public boolean tryAcquire(Train train) {
            if(super.tryAcquire()) {
                acquiredTrain = train;
                return true;
            }
            return false;
        }

        @Override
        public void release() {
            acquiredTrain = null;
            super.release();
        }
    }

    public class RailDeadend implements SensorAction{
        private final List<Sensor> associatedSensors;
        private final long waitMod;

        public RailDeadend(boolean isStation, Sensor... sensors){
            associatedSensors = List.of(sensors);
            if(isStation){
                waitMod = 1000;
            } else {
                waitMod = 200;
            }
        }

        @Override
        public List<Sensor> getAssociatedSensors() {
            return associatedSensors;
        }

        @Override
        public void act(Train train, int preferredState) {
            train.setSpeed(0);
            try {
                sleep(waitMod + Math.abs(train.startSpeed) * 20L);
            }catch (InterruptedException e){
                e.printStackTrace();
            }

            train.inverseSpeed();
        }
    }

    /**
     * An interface for linking multiple sensors to a single action.
     */
    public interface SensorAction {

        /**
         * Gets the associated sensors for this action
         * @return A list of sensor cordinates
         */
        List<Sensor> getAssociatedSensors();

        /**
         * The method that is called when a sensor event occurs.
         * @param train The train that caused the sensor event.
         */
        void act(Train train, int preferredState);

        default void act(Train train){
            act(train, 0);
        }
    }

    public static class Sensor{
         public final int preferredSwitchState;
         public final int x,y;

        Sensor(int x, int y, int switchState){
            this.x = x;
            this.y = y;
            this.preferredSwitchState = switchState;
        }

        Sensor(int x, int y){
            this.x = x;
            this.y = y;
            this.preferredSwitchState = 0;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Sensor sensor = (Sensor) o;
            return x == sensor.x && y == sensor.y;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y);
        }
    }
}
