package lab1;

import TSim.*;

import java.awt.*;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.Semaphore;

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
        //sensorActions.add(new RailSwitch());
        //sensorActions.add(new RailSwitch());
        //sensorActions.add(new RailSwitch());
        //sensorActions.add(new RailSwitch());

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
        final int startSpeed;
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
            try {
                while (true) {
                    SensorEvent sensor = tsi.getSensor(trainId);
                    System.out.println(sensor.toString());
                    Point2D pnt = new Point(sensor.getXpos(), sensor.getYpos());

                    var acts = sensorActions.stream().filter((a) -> a.getAssociatedSensors().contains(pnt));
                    for (Iterator<SensorAction> it = acts.iterator(); it.hasNext(); ) {
                        it.next().act(this);
                    }
                }
            } catch (CommandException | InterruptedException e) {
                e.printStackTrace();
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

        /**
         * Starts the train thread and sets the speed of the train to the default speed.
         * @see Thread#start()
         */
        @Override
        public synchronized void start() {
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
        private int switchState = 0x01;


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
        public void act(Train train, boolean opposite){

                try {
                    if(train.equals(acquiredTrain)){
                        toggleSwitch();
                        release();
                        return;
                    }

                    if(!tryAcquire()){
                        train.setSpeed(0);
                        acquire();
                    }

                    if(opposite && switchState != TSimInterface.SWITCH_LEFT){
                        toggleSwitch();
                    }

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
        void act(Train train, boolean opposite);

        default void act(Train train){
            act(train, false);
        }
    }

    public class Sensor{
         public final int prefferedSwitchState;
         public final int x,y;

        Sensor(int x, int y, int switchState){
            this.x = x;
            this.y = y;
            this.prefferedSwitchState = switchState;
        }

        Sensor(int x, int y){
            this.x = x;
            this.y = y;
            this.prefferedSwitchState = 0;
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
