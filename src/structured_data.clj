(ns structured-data)

(defn do-a-thing
  [x]
  (let [summa (+ x x)]
    (Math/pow summa summa)
    )
  )

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b)
    )
  )

(defn spiff-destructuring2 [a _ b]
    (+ a b)
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    )
  )

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1)
  )

(defn square? [rectangle]
  (if
    (= (width rectangle) (height rectangle))
    true
    false
    )
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2)
         (<= y1 py y2)
      )
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2)
      )
    )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (if
    (> (author-count book) 1)
    true
    false
    )
  )

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)
    )
  )

(defn alive? [author]
  (if
    (contains? author :death-year)
    false
    true
    )
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements
  "Take a vector of vectors and return a sequence of second elements."
  [collection]
  (let [get-second (fn [vector] (get vector 1))]
    (map get-second collection)
    )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (let [increasing (fn [seq] (apply <= a-seq))
        decreasing (fn [seq] (apply >= a-seq))]
    (cond
      (increasing a-seq) true
      (decreasing a-seq) true
      :else false
      )
    )
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if
    (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
  (let [size-seq (count a-seq)
        size-set (count (set a-seq))]
    (if
      (= size-seq size-set)
      false
      true
      )
    )
  )

(defn old-book->new-book [book]
  (let [authors (:authors book)
        new-authors (set authors)]
    (assoc book :authors new-authors)
    )
  )

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)
    )
  )

(defn authors [books]
  (let [all-authors (fn [books] (map :authors books))]
    (apply clojure.set/union (all-authors books))
    )
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [authorname (:name author)
        byear (:birth-year author)
        dyear (:death-year author)]
    (cond
      (contains? author :death-year) (str authorname " (" byear " - " dyear ")")
      (contains? author :birth-year) (str authorname " (" byear " - )")
      :else (str authorname)
      )
    )
  )

(defn authors->string [authors]
  (let [stringify (fn [authors] (map author->string authors))
        join (fn [authors] (apply str (interpose ", "(stringify authors))))]
    (cond
      (= (count authors) 0) ""
      (= (count authors) 1) (apply str (stringify authors))
      :else (join authors)
      )
    )
  )

(defn book->string [book]
  (let [stringify-name (:title book)
        written (str ", written by ")]
    (str stringify-name written (authors->string (:authors book)))
    )
  )

(defn books->string [books]
  (let [stringify-books (fn [books] (apply str (interpose ", " (map book->string books))))
        n (str (count books))]
    (cond
      (= (count books) 0) "No books."
      (= (count books) 1) (str n " book. " (str (book->string (first books)) "."))
      :else (str n " books. " (str (stringify-books books) "."))
      )
    )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (let [matches (filter (fn [author] (= name (:name author))) authors)]
    (if
      (= (count matches) 0) nil
      (first matches)
      )
    )
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (let [living (living-authors (:authors book))]
    (if
      (empty? living)
      false
      true
      )
    )
  )

(defn books-by-living-authors [books]
  (let [books-with-live-author (filter has-a-living-author? books)]
    books-with-live-author
    )
  )

; %________%
