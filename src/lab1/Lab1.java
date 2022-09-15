package lab1;

import TSim.*;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.Semaphore;

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
     *
     * @see RailSwitch
     */
    private final List<SensorAction> sensorActions = new ArrayList<>();


    /**
     * The constructor of the train control system. It initializes the TSimInterface and
     * indexes the rail elements. It also starts the threads that handle the trains.
     *
     * @param speed1 Speed of the first train.
     * @param speed2 Speed of the second train.
     */
    public Lab1(int speed1, int speed2) {
        tsi = TSimInterface.getInstance();

        Train t1 = new Train(1, speed1);
        Train t2 = new Train(2, speed2);

        RailSwitch rs1707 = new RailSwitch(17, 7,
            new Sensor(14, 7, SWITCH_RIGHT),
            new Sensor(14, 8, SWITCH_LEFT),
            new Sensor(19, 9, 0)
        );;

        sensorActions.add(new DualRailSemaphore(
            t1,
            rs1707,
            null,
            new Sensor(19, 9, SWITCH_LEFT)
        ));

        RailSwitch rs0409 = new RailSwitch(4, 9,
            new Sensor(1, 10, 0),
            new Sensor(7, 10, SWITCH_RIGHT),
            new Sensor(7, 9, SWITCH_LEFT)
        );

        RailSwitch rs1509 = new RailSwitch(15, 9,
            new Sensor(12, 9, SWITCH_RIGHT),
            new Sensor(12, 10, SWITCH_LEFT),
            new Sensor(19, 9, 0)
        );

        sensorActions.add(new DualRailSemaphore(
            null,
            rs0409,
            rs1509,
            new Sensor(19, 9, SWITCH_RIGHT),
            new Sensor(1, 10, SWITCH_LEFT)
        ));

        RailSwitch rs0311 = new RailSwitch(3, 11,
            new Sensor(1, 10, 0),
            new Sensor(6, 11, SWITCH_LEFT),
            new Sensor(6, 13, SWITCH_RIGHT)
        );

        sensorActions.add(new DualRailSemaphore(
            t2,
            null,
            rs0311,
            new Sensor(1, 10, SWITCH_RIGHT)
        ));

        // index the rail elements and sensors
        sensorActions.add(new RailSemaphore(
            new Sensor(14, 7),
            new Sensor(14, 8),
            new Sensor(12, 9),
            new Sensor(12, 10)
        ));
        sensorActions.add(new RailSemaphore(
            new Sensor(6, 6),
            new Sensor(8, 5),
            new Sensor(10, 7),
            new Sensor(10, 8)
        ));
        sensorActions.add(new RailSemaphore(
            new Sensor(7, 9),
            new Sensor(7, 10),
            new Sensor(6, 11),
            new Sensor(6, 13)
        ));

        sensorActions.add(rs1707);
        sensorActions.add(rs1509);
        sensorActions.add(rs0409);
        sensorActions.add(rs0311);


        sensorActions.add(new RailDeadend(false,
            new Sensor(14, 3)
        ));
        sensorActions.add(new RailDeadend(true,
            new Sensor(14, 5)
        ));
        sensorActions.add(new RailDeadend(false,
            new Sensor(14, 11)
        ));
        sensorActions.add(new RailDeadend(true,
            new Sensor(14, 13)
        ));

        // start trains
        t1.start();
        t2.start();

    }

    /**
     * The class that handles the trains and starts their own threads. It will also call a relevant
     * {@link SensorAction#act(Train)} when a sensor event occurs.
     */
    public class Train extends Thread implements Runnable {
        /**
         * The default speed of the train.
         */
        int startSpeed;
        /**
         * The id of the train.
         */
        final int trainId;

        /**
         * The constructor of the train. To start the train, call {@link #start()}.
         *
         * @param trainId    The id of the train.
         * @param startSpeed The default speed of the train.
         */
        Train(int trainId, int startSpeed) {

            this.startSpeed = startSpeed;
            this.trainId = trainId;
        }

        /**
         * It is called by thread.start() and watches for sensor events. When a sensor event occurs,
         * it calls {@link SensorAction#act(Train)} based on the sensor coordinates.
         *
         * @see Runnable#run()
         */
        @Override
        public void run() {
            while (true) {
                try {

                    SensorEvent sensor = tsi.getSensor(trainId);

                    if (sensor.getStatus() == SensorEvent.ACTIVE) {
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
         *
         * @see #setSpeed(int)
         */
        public void setSpeed() {
            setSpeed(startSpeed);
        }

        /**
         * Sets the speed of the train.
         *
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
            startSpeed = startSpeed * -1;
            setSpeed();
        }

        /**
         * Starts the train thread and sets the speed of the train to the default speed.
         *
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
         *
         * @see Thread#interrupt()
         */
        @Override
        public void interrupt() {
            try {
                tsi.setSpeed(trainId, 0);
            } catch (CommandException e) {
                e.printStackTrace();
            }
            System.out.println("Train " + trainId + " stopped.");

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
     *
     * @see SensorAction
     * @see Semaphore
     */
    public class RailSwitch extends Semaphore implements SensorAction {
        private List<Sensor> associatedSensors;
        private int x, y;
        private Train acquiredTrain = null;
        private int switchState = SWITCH_LEFT;


        /**
         * The constructor of the rail switch. It sets the number of permits to 1.
         * And saves the coordinates of the sensors.
         *
         * @param sensors the coordinates of the sensors that are connected to the switch.
         */
        public RailSwitch(int xpos, int ypos, Sensor... sensors) {
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
         *
         * @param train The train that caused the sensor event.
         */
        @Override
        public void act(Train train, int direction) {
            try {
                if (train.equals(acquiredTrain)) {
                    toggleSwitch();
                    release();
                    System.out.println("released (" + x + ',' + y + ") for " + train.trainId);
                    return;
                }

                if (!tryAcquire(train)) {
                    train.setSpeed(0);
                    System.out.println("waiting (" + x + ',' + y + ") for " + train.trainId);
                    acquire(train);
                    train.setSpeed();
                }

                if (direction > 0 && direction != switchState) {
                    setSwitchState(direction);
                    System.out.println("switched " + train.trainId + " to " + direction);
                }

                System.out.println("acquired (" + x + ',' + y + ") for " + train.trainId);

            } catch (CommandException e) {
                e.printStackTrace();
                release();
            } catch (InterruptedException e) {
                e.printStackTrace();
                train.interrupt();
            }
        }

        public void setSwitchState(int switchState) throws CommandException {
            this.switchState = switchState;
            tsi.setSwitch(x, y, switchState);
        }

        public void toggleSwitch() throws CommandException {
            if (switchState == 0x01) {
                switchState = 0x02;
            } else {
                switchState = 0x01;
            }

            tsi.setSwitch(x, y, switchState);
            System.out.println("switched (" + x + ',' + y + ") to " + switchState);
        }


        public void acquire(Train train) throws InterruptedException {
            super.acquire();
            acquiredTrain = train;
        }

        public boolean tryAcquire(Train train) {
            if (super.tryAcquire()) {
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
         *
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
        public void act(Train train, int direction) {
            try {
                if (train.equals(acquiredTrain)) {
                    release();
                    System.out.println("released semaphore " + associatedSensors.toString() + " for " + train.trainId);
                    return;
                }

                if (!tryAcquire(train)) {
                    train.setSpeed(0);
                    System.out.println("waiting semaphore " + associatedSensors.toString() + " for " + train.trainId);
                    acquire(train);
                    train.setSpeed();
                }

                System.out.println("acquired semaphore " + associatedSensors.toString() + " for " + train.trainId);


            }/*catch (CommandException e){
                e.printStackTrace();
                release();
            }*/ catch (InterruptedException e) {
                e.printStackTrace();
                train.interrupt();
            }
        }

        public void acquire(Train train) throws InterruptedException {
            super.acquire();
            acquiredTrain = train;
        }

        public boolean tryAcquire(Train train) {
            if (super.tryAcquire()) {
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

    public class RailDeadend implements SensorAction {
        private final List<Sensor> associatedSensors;
        private final long waitMod;
        private boolean initialLock = false;

        public RailDeadend(boolean isStation, Sensor... sensors) {
            associatedSensors = List.of(sensors);
            if (isStation) {
                waitMod = 1000;
            } else {
                waitMod = 600;
                initialLock = true;
            }
        }

        @Override
        public List<Sensor> getAssociatedSensors() {
            return associatedSensors;
        }

        @Override
        public void act(Train train, int direction) {
            if(initialLock){
                initialLock = false;
                return;
            }

            train.setSpeed(0);
            try {
                sleep(waitMod + Math.abs(train.startSpeed) * 20L);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            train.inverseSpeed();
            System.out.println("reversed " + train.trainId + " to " + train.startSpeed + " at " + associatedSensors.toString());
        }
    }

    /**
     * An interface for linking multiple sensors to a single action.
     */
    public interface SensorAction {

        /**
         * Gets the associated sensors for this action
         *
         * @return A list of sensor cordinates
         */
        List<Sensor> getAssociatedSensors();

        /**
         * The method that is called when a sensor event occurs.
         *
         * @param train The train that caused the sensor event.
         */
        void act(Train train, int direction);

        default void act(Train train) {
            act(train, 0);
        }
    }

    public static class Sensor {
        public final int preferredSwitchState;
        public final int x, y;

        Sensor(int x, int y, int switchState) {
            this.x = x;
            this.y = y;
            this.preferredSwitchState = switchState;
        }

        Sensor(int x, int y) {
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

        @Override
        public String toString() {
            return "(" + x + ", " + y + ')';
        }
    }

    public class DualRailSemaphore extends Semaphore implements SensorAction {
        private final List<Sensor> associatedSensors;
        private final RailSwitch rs1, rs2;
        private Train acquiredTrain = null;
        private boolean changeLock = false;
        private Train intialTrain = null;

        public DualRailSemaphore(Train intialTrain, RailSwitch rs1, RailSwitch rs2, Sensor... sensors) {
            super(1);
            this.rs1 = rs1;
            this.rs2 = rs2;
            this.intialTrain = intialTrain;
            associatedSensors = List.of(sensors);
        }

        @Override
        public List<Sensor> getAssociatedSensors() {
            return associatedSensors;
        }

        @Override
        public void act(Train train, int direction) {
            if(train.equals(acquiredTrain)){
                release();
                System.out.println("released dual semaphore " + associatedSensors.toString() + " for " + train.trainId);
                return;
            }

            if(intialTrain == train){
                intialTrain = null;
                return;
            }

            if(changeLock){
                changeLock = false;
                System.out.println("unlocked dual semaphore " + associatedSensors.toString() + " for " + train.trainId);
                return;
            }

            try {
                if (rs1 != null && direction == SWITCH_LEFT) {
                    if (tryAcquire(train)) {
                        rs1.setSwitchState(SWITCH_LEFT);
                    } else{
                        rs1.setSwitchState(SWITCH_RIGHT);
                        System.out.println("switched dual semaphore " + associatedSensors.toString() + " for " + train.trainId);
                        changeLock = true;
                    }
                } else if(rs2 != null && direction == SWITCH_RIGHT){
                    if (tryAcquire(train)) {
                        rs2.setSwitchState(SWITCH_RIGHT);
                    } else{
                        rs2.setSwitchState(SWITCH_LEFT);
                        System.out.println("switched dual semaphore " + associatedSensors.toString() + " for " + train.trainId);
                        changeLock = true;
                    }
                }
            } catch (CommandException e) {
                e.printStackTrace();
            }
        }
        public static int getReverseDirection(int direction){
            if (direction == SWITCH_LEFT) {
                return SWITCH_RIGHT;
            } else {
                return SWITCH_LEFT;
            }
        }

        public boolean tryAcquire(Train train) {
            if(super.tryAcquire()){
                acquiredTrain = train;
                System.out.println("acquired dual semaphore " + associatedSensors.toString() + " for " + train.trainId);
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
}
