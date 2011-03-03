package org.karatachi.java.garbage;

import java.util.*;

public class Profiler {
    public static void main(String[] args) {
        JavaNativeProfile.run();
    }

    private static final int repeat = 10000;
    private static final int trials = 100;
    private static final int truncate = trials / 5;

    public static void profile(String title, Runnable block) {
        List<Long> result = new ArrayList<Long>();

        for (int i = 0; i < trials; ++i) {
            long start = System.nanoTime();
            block.run();
            long end = System.nanoTime();
            result.add(end - start);
        }

        long totalTime = 0;
        Collections.sort(result);
        for (Long time : result.subList(truncate, trials - truncate)) {
            totalTime += time;
        }
        long average = totalTime / (trials - truncate*2) / 1000;

        System.out.println(String.format("%s: %d micro sec", title, average));
    }

    public static class JavaNativeProfile {
        private static String[] array = new String[repeat];
        static { Arrays.fill(array, "foo"); }
        private static List<String> arraylist = new ArrayList<String>(Arrays.asList(array));
        private static List<String> linkedlist = new LinkedList<String>(Arrays.asList(array));

        private static String field = "bar";

        public static void run() {
            profileApply();
            profileUpdate();
            profileAppend();
        }

        private static void profileApply() {
            profile("Object[].apply", new Runnable() {
                    public void run() {
                        for (int i = 0; i < repeat; ++i) field = array[i];
                    }
                });
            profile("java.util.ArrayList<Object>.apply", new Runnable() {
                    public void run() {
                        for (int i = 0; i < repeat; ++i) field = arraylist.get(i);
                    }
                });
            profile("java.util.LinkedList<Object>.apply", new Runnable() {
                    public void run() {
                        for (int i = 0; i < repeat; ++i) field = linkedlist.get(i);
                    }
                });
        }

        private static void profileUpdate() {
            profile("Object[].update", new Runnable() {
                    public void run() {
                        for (int i = 0; i < repeat; ++i) array[i] = field;
                    }
                });
            profile("java.util.ArrayList<Object>.update", new Runnable() {
                    public void run() {
                        for (int i = 0; i < repeat; ++i) arraylist.set(i, field);
                    }
                });
            profile("java.util.LinkedList<Object>.update", new Runnable() {
                    public void run() {
                        for (int i = 0; i < repeat; ++i) linkedlist.set(i, field);
                    }
                });
        }

        private static void profileAppend() {
            profile("java.util.ArrayList<Object>.append", new Runnable() {
                    public void run() {
                        List<String> arraylist = new ArrayList<String>(Arrays.asList(array));
                        for (int i = 0; i < repeat; ++i) arraylist.add(field);
                    }
                });
            profile("java.util.LinkedList<Object>.append", new Runnable() {
                    public void run() {
                        List<String> linkedlist = new LinkedList<String>(Arrays.asList(array));
                        for (int i = 0; i < repeat; ++i) linkedlist.add(field);
                    }
                });
        }
    }
}
