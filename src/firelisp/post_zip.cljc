(ns firelisp.post-zip
  (:require [clojure.zip :as z]))

(defn leftmost-descendant
  "Given a zipper loc, returns its leftmost descendent (ie, down repeatedly)."
  [loc]
  (if (and (z/branch? loc) (z/down loc))
    (recur (z/down loc))
    loc))

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
          (z/up loc)))))

(defn root
  "zips all the way up and returns the root node, reflecting any
 changes."
  {:added "1.0"}
  [loc]
  (if (= :end (loc 1))
    loc
    (let [p (z/up loc)]
      (if p
        (recur p)
        loc))))