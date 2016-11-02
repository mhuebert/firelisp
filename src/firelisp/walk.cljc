(ns firelisp.walk
  (:require [clojure.zip :as z]))

(def ^:dynamic *walk-state* nil)

(defn wrap [f pre-f]
  (fn [& args]
    (apply pre-f args)
    (apply f args)))

(def up (wrap z/up (fn [loc]
                     (swap! *walk-state* #(-> %
                                              (update :depth dec)
                                              (update :operators pop))))))
(def down (wrap z/down (fn [loc]
                         (let [operator (-> loc
                                            z/down
                                            z/leftmost
                                            z/node)
                               sym? (symbol? operator)]
                           (swap! *walk-state* #(-> %
                                                    (update :depth inc)
                                                    (update :operators conj (if sym? operator nil))))))))

(defn leftmost-descendant
  "Given a zipper loc, returns its leftmost descendent (ie, down repeatedly)."
  ([loc] (leftmost-descendant loc true))
  ([loc track-depth?]
   (let [down (if track-depth? down z/down)]
     (if (and (z/branch? loc) (z/down loc))
       (recur (down loc) track-depth?)
       loc))))

(defn postorder-first
  "Given a root node, returns the first node of a postorder tree walk. See
   comment on postorder-next."
  [loc]
  (leftmost-descendant loc))


(defn postorder-next
  "Moves to the next loc in the hierarchy in postorder traversal. Behaves like
   clojure.z/next otherwise. Note that unlike with a pre-order walk, the root
   is NOT the first element in the walk order, so be sure to take that into
   account in your algorithm if it matters (ie, call postorder-first first
   thing before processing a node)."
  [loc]
  (if (= :end (loc 1))                                      ;; If it's the end, return the end.
    loc
    (if (nil? (z/up loc))
      ;; Node has no parent, this is the root/final node, return the end.
      [(z/node loc) :end]
      ;; Node is internal, so we got to it by having traversed its children.
      ;; Instead, we want to try to move to the leftmost descendant of our
      ;; right sibling, if possible.
      (or (and (z/right loc) (leftmost-descendant (z/right loc)))
          ;; There was no right sibling, we must move back up the tree.
          (up loc)))))

(defn postorder-replace [f expr]
  (binding [*walk-state* (atom {:depth     0
                                :context   {}
                                :operators []})]
    (loop [loc (postorder-first (z/seq-zip expr))]
      (if (z/end? loc)
        (z/root loc)
        (recur (-> loc
                   (z/replace (f (z/node loc) @*walk-state*))
                   (postorder-next)))))))