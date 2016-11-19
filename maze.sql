--=======================================================--
-- SQL Maze By Jonathan Gough <jonathanpgough@gmail.com> --
--                                                       --
--   This single SQL query uses recursive common table   --
--     expressions to generate a maze then render it.    --
--                                                       --
------> No procedures, no user functions, no schema <------
--                                                       --
-- Uses a combination of recursive backtracking combined --
--         with a random spanning tree algorithm         --
--=======================================================--

WITH RECURSIVE

-- Set up some variables.  Change these if you wish
vars(size, recursive_backtrack_probability) AS (
  SELECT
    20, -- Size of the maze (1-25)
    0.1 -- Probability of following recursive backtracking algorithm instead of random spanning tree (0-1)
        --   (set to 1 for full recursive backtracking)
        --   (set to 0 for random spanning tree)
),

-- Initialise the maze nodes (junctions)
nodes(id, x, y) AS (
  SELECT row_number() OVER(ORDER BY y,x) id, (x*2)+1, (y*2)+1
    FROM generate_series(0,(SELECT size-1 FROM vars)) x,
         generate_series(0,(SELECT size-1 FROM vars)) y
),

-- Initialise all possible edges in the maze
edges(id, f, t) AS (
  SELECT row_number() OVER(ORDER BY f,t)::INT id,f, t
    FROM generate_series(1,(SELECT size*size FROM vars)) f,
         generate_series(1,(SELECT size*size FROM vars)) t
   WHERE (t=f+1 AND f%(SELECT size FROM vars) != 0) -- Edge Right
      OR (t=f+(SELECT size FROM vars)
          AND f<=(SELECT size*size-size FROM vars)) -- Edge Down
      OR (f=t+1 AND t%(SELECT size FROM vars) != 0) -- Edge Left
      OR (f=t+(SELECT size FROM vars)
          AND t<=(SELECT size*size-size FROM vars)) -- Edge Up
),

-- Incrementally add edges to the maze until all nodes visited
build_maze(visited, edges, step) AS (
  -- Initialise
  SELECT ARRAY[1],           -- Start node
         ARRAY[]::INTEGER[], -- No edges visited yet
         1
  UNION ALL
  (
    -- Recurse, and find an unconnected node to attach
    SELECT visited || t,                   -- add this node to the visited array
           edges || edges.id,              -- add this edge to the included edges array
           step+1 FROM build_maze
      JOIN edges ON NOT (t = ANY(visited)) -- find an edge TO any non-visited node 
                    AND f = ANY(visited)   --          but FROM any visited node
                    AND f != (SELECT size*size FROM vars) -- (but not FROM the exit, that looks bad)
  ORDER BY CASE WHEN RANDOM()<(SELECT recursive_backtrack_probability FROM vars)
                THEN array_position(visited,f) -- order by the last visited (recursive backtrack)
                ELSE RANDOM()                  -- or pick a visited node at random (random spanning tree)
           END DESC,
           RANDOM() -- in case of a tie for recursive backtrack, pick randomly
     LIMIT 1
  )
),

-- Only return the edges that are in the maze
maze_edges(f, t) AS (
  SELECT f, t FROM (
    SELECT unnest(edges)
    FROM (SELECT * FROM build_maze ORDER BY step DESC LIMIT 1) a
  ) a
  JOIN edges ON unnest=edges.id
),

-- Construct a "Base maze", with walls everywhere apart from junctions
base_maze(x, y, c) AS (
 SELECT x, y,
   CASE WHEN (
               (y=0 AND x=1)
                OR
               (y=(SELECT (size*2) FROM vars)
                  AND x=(SELECT (size*2)-1 FROM vars))
             ) THEN '↓↓' -- Entrance and Exit
        WHEN (x%2=0 OR y%2=0)
         AND (x!=1 OR y!=0)
         THEN '██' -- A wall
     ELSE '  ' -- A gap
   END::VARCHAR
 FROM generate_series(0,(SELECT (size*2) FROM vars)) x,
      generate_series(0,(SELECT (size*2) FROM vars)) y
),

-- Find where the edge gaps in the walls will be as x,y pairs
gap_locations(x, y, c) AS (
  SELECT (f.x+t.x)/2 x,
         (f.y+t.y)/2 y,
         '  '::VARCHAR c
  FROM maze_edges
  JOIN nodes f ON maze_edges.f=f.id
  JOIN nodes t ON maze_edges.t=t.id
),

-- Combine the base maze with the wall gaps
combined_maze(x, y, c) AS (
  SELECT x, y, MIN(c) FROM (
    SELECT * FROM base_maze
    UNION
    SELECT * FROM gap_locations
  ) combined_maze
  GROUP BY x, y
),

-- Format the maze as a string
maze_to_string(r) AS (
  SELECT array_to_string(array_agg(c ORDER BY x),'')
  FROM (SELECT x, y, c FROM combined_maze) combined_maze
  GROUP BY y
  ORDER BY y
)

-- Output maze
SELECT r "Amazing SQL Maze" FROM maze_to_string

