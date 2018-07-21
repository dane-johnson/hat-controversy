(ns hat-controversy.core
  (:require [reagent.core :refer [render atom]]))

(def instructors '["Dolphin" "DK" "Shuckle" "Cordouroy" "Storm Queen" "Squirrel"
                   "Kit" "Slim Jim" "Tweety" "Pikachu"])

(defn index-rightmost-increase
  [order]
  (if (< (count order) 2) -1
      (let [[next-last last] (take-last 2 order)]
        (if (> last next-last) (- (count order) 2)
            (recur (butlast order))))))

(defn smallest-available-greater-than
  [used prev]
  (loop [i (inc prev)]
    (if-not (contains? used i) i
            (recur (inc i)))))

(defn next-order
  [prev-order]
  (if-not (neg? (index-rightmost-increase prev-order))
    (let [i (index-rightmost-increase prev-order)
          head (take i prev-order)
          middle (smallest-available-greater-than (into #{} (take (inc i) prev-order)) (get prev-order i))
          tail (filter #(not (contains? (into #{} (conj head middle)) %1)) (range (count prev-order)))]
      (vec (concat head [middle] tail)))))

(defn instructor-permutations
  ([] (instructor-permutations (vec (range (count instructors)))))
  ([prev] (lazy-seq (cons (map #(get instructors %) prev)
                          (instructor-permutations (next-order prev))))))

(defn instructor-permutations-generator
  []
  (let [curr (atom (vec (range (count instructors))))]
    (fn [] (let [my-curr @curr]
             (reset! curr (next-order my-curr))
             (map #(get instructors %) my-curr)))))

(defonce generator (instructor-permutations-generator))
(defonce permutations (atom (vec (repeatedly 100 generator))))

(defn tables
  []
  [:div [:ol (map (fn [hat-list]
                            [:li
                             [:table [:tr [:th "Instructor"] [:th "Hat"]]
                              (map (fn [instructor hat] [:tr [:td instructor] [:td hat]]) instructors hat-list)]])
                          @permutations)]])

(set! (.-onscroll js/window) 
      #(if (>= (+ (.. js/window -scrollY) (.. js/window -screen -height))
              (.. js/document (getElementById "root") -scrollHeight))
         (swap! permutations concat (vec (repeatedly 100 generator)))))

(render [tables] (.getElementById js/document "root"))
