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
    /** the player id, is -1 when not set */
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

    /**
     * A constrcutor for passing premade states to a recursive task
     * @param maze the maze to search
     * @param visited a pointer to the visited nodes
     * @param predecessor a pointer or copy to the previous path
     * @param frontier the stack of future nodes to visited, may be an empty stack
     * @param forkAfter the amounts of nodes to visited before forking
     */
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
        // check if the start node is already visited otherwise end the thread
        if(visited.contains(start)){
            return null;
        }
        //check if thread already has a player defined otherwise make a new one
        if(player == -1) {
            player = maze.newPlayer(start);
        }

        frontier.push(start);
        System.out.println(this + " starts at " + start);
        int count = 0; // counter used for forkafter



        while (!frontier.empty()) {
            // get the new node to process
            int current = frontier.pop();

            // if already visited skip to next iteration
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

            // get neighbors that are not previously visited
            List<Integer> n = maze.neighbors(current).stream().filter(e -> !visited.contains(e)).toList();
            System.out.println(this + " has " + n.size() + " neighbors");

            // if there are multiple neighbors and fork is allowed then fork
            if(n.size() > 1 && count > forkAfter) {
                count = 0;
                List<ForkJoinSolver> tasks = new ArrayList<>();
                // setup forked threads
                for (int ni: n){
                        predecessor.put(ni, current);
                        ForkJoinSolver task = new ForkJoinSolver(maze, visited, (Map<Integer, Integer>) ((HashMap) predecessor).clone(), new Stack<>(), forkAfter);
                        task.start = ni;
                        if(player != -1) {
                            task.player = player;
                            player = -1;
                        }
                        tasks.add(task);
                }
                // start the threads
                for (ForkJoinSolver task: tasks) {
                    task.fork();
                }

                // find a suitable result if any and return it
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
            // if fork not permitted then just add all neighbors to frontier
            else  if(n.size() > 1){
                for(int ni: n){
                    predecessor.put(ni, current);
                    frontier.push(ni);
                }
            }
            // add a single node to frontier
            else if(n.size() == 1) {
                int next = n.get(0);
                predecessor.put(next, current);
                frontier.push(next);
            }

        }

        // on fail
        System.out.println(this + " found no path");
        return null;
    }


    /**
     * searches trough the predecessor to find the true start point
     * @param goal the goal node
     * @return the path from start to goal
     */
    protected List<Integer> pathFromTo(int goal) {
        System.out.println(this + " reconstructing path from " + goal + ": " + predecessor);
        int current = goal;
        while (predecessor.containsKey(current)) {
            current = predecessor.get(current);
        }
        return super.pathFromTo(current, goal);
    }
}
