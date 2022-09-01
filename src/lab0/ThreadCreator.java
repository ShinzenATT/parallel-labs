package lab0;

import java.util.ArrayList;
import java.util.List;

public class ThreadCreator{
    public static void main(String[] args){
        List threads = new ArrayList<Thread>();

        // create actions
        for (int i = 0; i < 50; i++) {
            final int a = i;
            threads.add(new Thread(() -> {
                System.out.println(a);
                System.out.println(a);
                System.out.println(a);
                System.out.println(a);
                System.out.println(a);
            }));
        }

        // run all threads
        for (Object t: threads){
            ((Thread) t).start();
        }
    }
}
