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
     * @see RailSemaphore
     * @see DualRailSemaphore
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

        // Set up the trains
        Train t1 = new Train(1, speed1);
        Train t2 = new Train(2, speed2);

        // Set up Railswitches depended by DualRailSemaphore and add DualRailSemaphores to list
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

        // add all RailSemaphores
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

        // Add all RailSwitches
        sensorActions.add(rs1707);
        sensorActions.add(rs1509);
        sensorActions.add(rs0409);
        sensorActions.add(rs0311);


        // Add all RailDeadends
        sensorActions.add(new RailDeadEnd(false,
            new Sensor(14, 3)
        ));
        sensorActions.add(new RailDeadEnd(true,
            new Sensor(14, 5)
        ));
        sensorActions.add(new RailDeadEnd(false,
            new Sensor(14, 11)
        ));
        sensorActions.add(new RailDeadEnd(true,
            new Sensor(14, 13)
        ));

        // start trains
        t1.start();
        t2.start();

    }

    /**
     * The class that handles the trains and starts their own threads. It will also call a relevant
     * {@link SensorAction#act(Train) SensorAction} when a sensor event occurs.
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

                        // filter out the list to only those that have the sensor associated and execute them
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

        /**
         * Makes the train go in reverse which effectively results in {@link Train#startSpeed startSpeed} being
         * multiplied by -1. For this to work the train needs to have its speed to 0.
         * @see Train#setSpeed(int)
         */
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

        /**
         * Check if the train id and class matches with the other object.
         * @param o the object to be checked against
         * @return a boolean that is true when the trainId is matched
         */
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Train train = (Train) o;
            return trainId == train.trainId;
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
        /** the sensors to trigger the railswitch, sensors may contain preferred switch state */
        private final List<Sensor> associatedSensors;
        /** the cordinates of the switch */
        private final int x, y;
        /** the train that has acquired the resource, used to check if a train is exiting */
        private Train acquiredTrain = null;
        /** the current state of the switch */
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
         * It is called when a sensor event occurs. It changes the switch state based on where the rain enters and stops
         * other trains from entering until the switch is released.
         *
         * @param train The train that caused the sensor event.
         * @param direction the preferred switch state, if it is not 0 then it will change the switch to that state.
         */
        @Override
        public void act(Train train, int direction) {
            try {
                // release the switch and toggle it
                if (train.equals(acquiredTrain)) {
                    toggleSwitch();
                    release();
                    System.out.println("released (" + x + ',' + y + ") for " + train.trainId);
                    return;
                }

                // try to acquire the switch, on fail stop the train and wait for it to be available
                if (!tryAcquire(train)) {
                    train.setSpeed(0);
                    System.out.println("waiting (" + x + ',' + y + ") for " + train.trainId);
                    acquire(train);
                    train.setSpeed();
                }

                // 
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

        /**
         * Sets the switch state to the provided argument by sending it to the simulation and saves it in this object.
         * @param switchState the state to change the switch to
         * @throws CommandException is thrown when the simulation gives an error such as train on switch.
         */
        public void setSwitchState(int switchState) throws CommandException {
            this.switchState = switchState;
            tsi.setSwitch(x, y, switchState);
        }

        /**
         * Toggles the switch to the opposite state, it the saved state is unknown then it defaults to
         * {@link TSimInterface#SWITCH_LEFT SWITCH_LEFT}.
         * @throws CommandException is thrown when simulation has an error such as train on switch.
         */
        public void toggleSwitch() throws CommandException {
            if (switchState == SWITCH_LEFT) {
                switchState = SWITCH_RIGHT;
            } else {
                switchState = SWITCH_LEFT;
            }

            tsi.setSwitch(x, y, switchState);
            System.out.println("switched (" + x + ',' + y + ") to " + switchState);
        }


        /**
         * Acquires the switch and saves which train that has acquired it. If no permits are available then the thread
         * will be put to sleep until the switch is released.
         * @param train the train that acquired the switch
         * @throws InterruptedException is thrown when the thread is interrupted
         * @see Semaphore#acquire()
         */
        public void acquire(Train train) throws InterruptedException {
            super.acquire();
            acquiredTrain = train;
        }

        /**
         * Tries to acquire the switch, it will return true when the switch is acquired or false on fail.
         * @param train the train that wants to acquire the switch
         * @return returns true when acquiring the switch is successful otherwise false.
         * @see Semaphore#tryAcquire()
         */
        public boolean tryAcquire(Train train) {
            if (super.tryAcquire()) {
                acquiredTrain = train;
                return true;
            }
            return false;
        }

        /**
         * Releases the switch and allows the switch to be acquired by another train.
         * @see Semaphore#release()
         */
        @Override
        public void release() {
            acquiredTrain = null;
            super.release();
        }
    }

    /**
     * A {@link SensorAction SensorAction} to handle crossings or paths that only one train can pass at the time.
     * @see Semaphore
     */
    public class RailSemaphore extends Semaphore implements SensorAction {
        /** The list of sensors to track enter and exit of trains */
        private List<Sensor> associatedSensors;
        /** the train that has acquired the resource */
        private Train acquiredTrain = null;

        /**
         * The constructor of the rail semaphore. It sets the number of permits to 1.
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

        /**
         * Is called when a sensor event occurs. It will have the train acquire the semaphore until it has passed trough.
         * If it fails to acquire then the train will stop and wait until it's available.
         * @param train The train that caused the sensor event.
         * @param direction Unused parameter
         */
        @Override
        public void act(Train train, int direction) {
            try {
                // release the semaphore when passed
                if (train.equals(acquiredTrain)) {
                    release();
                    System.out.println("released semaphore " + associatedSensors.toString() + " for " + train.trainId);
                    return;
                }

                // tries to acquire, on fail the train stops and waits until its available
                if (!tryAcquire(train)) {
                    train.setSpeed(0);
                    System.out.println("waiting semaphore " + associatedSensors.toString() + " for " + train.trainId);
                    acquire(train);
                    train.setSpeed();
                }

                System.out.println("acquired semaphore " + associatedSensors.toString() + " for " + train.trainId);


            } catch (InterruptedException e) {
                e.printStackTrace();
                train.interrupt();
            }
        }

        /**
         * Acquires the semaphore, if none is available then the thread will be put to sleep until it's available.
         * @param train the train that wants to acquire the semaphore
         * @throws InterruptedException is thrown when the thread is interrupted
         * @see Semaphore#acquire()
         */
        public void acquire(Train train) throws InterruptedException {
            super.acquire();
            acquiredTrain = train;
        }

        /**
         * Tries to acquire the semaphore and returns true on success. Otherwise, false is returned.
         * @param train the train that wants to acquire the semaphore
         * @return true when the semaphore is acquired otherwise false
         * @see Semaphore#tryAcquire()
         */
        public boolean tryAcquire(Train train) {
            if (super.tryAcquire()) {
                acquiredTrain = train;
                return true;
            }
            return false;
        }

        /**
         * Releases the semaphore when the train exits and lets another train to acquire.
         * @see Semaphore#release()
         */
        @Override
        public void release() {
            acquiredTrain = null;
            super.release();
        }
    }

    /**
     * A {@link SensorAction SensorAction} for making a train switch directions.
     */
    public class RailDeadEnd implements SensorAction {
        /** the sensor(s) for triggering this action */
        private final List<Sensor> associatedSensors;
        /** the sleep duration modifier based on the isStation parameter in the constructor */
        private final long waitMod;
        /** */
        private boolean initialLock = false;

        public RailDeadEnd(boolean isStation, Sensor... sensors) {
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
