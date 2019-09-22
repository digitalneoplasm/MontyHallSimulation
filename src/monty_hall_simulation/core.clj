;; Author: Daniel R. Schlegel
;; Simulation of the Monty Hall Problem.
;; Created 12/4/2015
;; Modified 9/22/2019

(ns monty-hall-simulation.core)

(def simulation-cycles 1000000)

(defn create-doors
  "Create a representation of the three doors with the car in a random spot.
   The three doors are represented as a vector of arity 3, with 0 in a slot
   meaning goat, and 1 meaning car."
  []
  (assoc [0 0 0] (rand-int 3) 1))
  
(defn choose-door
  "Generate a random integer 0, 1 or 2 which indicates the door to choose."[]
  (rand-int 3))

(defn reveal-goat
  "From the vector of doors and the selected index, returns the index of the
   reveled goat. If the selected-index is the car, this is, randomly, one of
   the remaining two doors. If the selected-index is a goat, it is the other
   index of the other goat."
  [doors selected-index]
  (let [unselected-idxes (remove #{selected-index} (range 3))]
	  (cond 
	    ;; Selected a car -> randomly pick one of the other two indices to reveal.
	    (= (nth doors selected-index) 1)
      (nth unselected-idxes (rand-int 2))
	    ;; Selected a goat -> reveal the other goat.
	    :default
	    (if (= (nth doors (first unselected-idxes)) 0)
       (first unselected-idxes)
       (second unselected-idxes)))))

(defn monty-hall
  "Run the monty-hall simulation one time. Returns 1 for car or 0 for goat
   indicating what was behind the chosen door. change-guess? instructs the
   simulation to change the guess after the first reveal, or not to change
   the guess."
  [change-guess?]
  (let [doors (create-doors)
        choice-idx (choose-door)
        revealed-goat-idx (reveal-goat doors choice-idx)
        choice-idx (if change-guess?
                     (first (remove #{choice-idx revealed-goat-idx} (range 3)))
                     choice-idx)]
    ;; Return what's behind the chosen door.
    (nth doors choice-idx)))

(defn simulate
  "Run the simulation the number of times indicated in the simulation-cycles
   global variable. Return the percent of time the car was selected."
  [change-guess?]
  (if change-guess?
    (println "Monty Hall Simulator - Changing Guess on Reveal -" simulation-cycles "iterations")
    (println "Monty Hall Simulator - Not Changing Guess on Reveal -" simulation-cycles "iterations"))

  (float (/ (count (filter #{1} 
                           (take simulation-cycles 
                                 (repeatedly #(monty-hall change-guess?)))))
            simulation-cycles)))
