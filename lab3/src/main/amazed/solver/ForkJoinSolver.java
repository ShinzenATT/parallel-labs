package amazed.solver;

import amazed.maze.Maze;
import jdk.jshell.spi.ExecutionControl;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ForkJoinPool;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver
{
    private int player = -1;
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze) { super(maze); }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;

    }

    private ForkJoinSolver(Maze maze, Set<Integer> visited, Map<Integer, Integer> predecessor, Stack<Integer> frontier, int forkAfter) {
        this(maze);
        this.visited = visited;
        this.predecessor = predecessor;
        this.frontier = frontier;
        this.forkAfter = forkAfter;
    }

    @Override
    protected void initStructures() {
        visited = new ConcurrentSkipListSet<>();
        predecessor = new HashMap<>();
        frontier = new Stack<>();
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    private List<Integer> parallelSearch()
    {
        if(player == -1) {
            player = maze.newPlayer(start);
        }
        frontier.push(start);
        System.out.println(this + " starts at " + start);
        int count = 0;



        while (!frontier.empty()) {
            // get the new node to process
            int current = frontier.pop();

            if (visited.contains(current)) {
                continue;
            }

            visited.add(current);
            maze.move(player, current);
            count++;
            System.out.println(this + " moved to " + current);

            if (maze.hasGoal(current)) {
                // search finished: reconstruct and return path
                System.out.println("!! " + this + " found goal at " + current);
                return pathFromTo(current);
            }

            List<Integer> n = maze.neighbors(current).stream().filter(e -> !visited.contains(e)).toList();
            System.out.println(this + " has " + n.size() + " neighbors");
            if(n.size() > 1 && count > forkAfter) {

                List<ForkJoinSolver> tasks = new ArrayList<>();
                for (int ni: n){
                    if (!visited.contains(ni)) {
                        predecessor.put(ni, current);
                        ForkJoinSolver task = new ForkJoinSolver(maze, visited, (Map<Integer, Integer>) ((HashMap) predecessor).clone(), new Stack<>(), forkAfter);
                        task.start = ni;
                        if(player != -1) {
                            task.player = player;
                            player = -1;
                        }
                        tasks.add(task);
                    }
                }
                for (ForkJoinSolver task: tasks) {
                    task.fork();
                }
                List<Integer> results = null;
                for (ForkJoinSolver task: tasks) {
                    var r = task.join();
                    System.out.println(this + " got result from " + task.start + ": " + r);
                    if(results != null && r != null && r.size() < results.size()) {
                        results = r;
                    } else if (r != null && results == null) {
                        results = r;
                    }
                }
                return results;
            }
            else  if(n.size() > 1){
                for(int ni: n){
                    predecessor.put(ni, current);
                    frontier.push(ni);
                }
            }
            else if(n.size() == 1) {
                int next = n.get(0);
                predecessor.put(next, current);
                frontier.push(next);
            }

        }

        System.out.println(this + " found no path");
        return null;
    }


    protected List<Integer> pathFromTo(int goal) {
        System.out.println(this + " reconstructing path from " + goal + ": " + predecessor);
        int current = goal;
        while (predecessor.containsKey(current)) {
            current = predecessor.get(current);
        }
        return super.pathFromTo(current, goal);
    }
}
