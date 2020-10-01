(ns machine-simulator)

(def tag first)
(defn tag-of? [sym s] (= sym (tag s)))

; build instructions
; -------------

(defn extract-label->idx [raw-instructions]
  (second
    (reduce
      (fn [[idx label->idx] part]
        (if (symbol? part)
          [idx
           (assoc label->idx part idx)]
          [(inc idx)
           label->idx]))
      [0 {}]
      raw-instructions)))

(defn extract-instructions [raw-instructions]
  (remove symbol? raw-instructions))

(comment
  (let [raw-ins '((assign foo (const 1))
                  label-one
                  (test (op =) (const 1) (reg foo))
                  (branch (label label-two))
                  label-two
                  (assign bar (const 2)))]
    [(extract-label->idx raw-ins)
     (map-indexed vector (extract-instructions raw-ins))]))


; parse-primitive
; ---------------

(defn parse-primitive [{:keys [registry-map label->idx] :as _data}
                       prim-exp]
  (let [res (condp tag-of? prim-exp
              'const
              (second prim-exp)
              'reg
              (get registry-map (second prim-exp))
              'label
              (label->idx (second prim-exp)))]
    res))

(comment
  (parse-primitive
    {:registry-map {'foo 1}}
    '(const 3))
  (parse-primitive
    {:registry-map {'foo 2}}
    '(reg foo))
  (parse-primitive
    {:registry-map {'foo 0} :label->idx {'label-one 1}}
    '(label label-one)))

; parse-operation
; ---------------

(def operation-sym (comp second first))
(def operation-args rest)

(defn parse-operation [{:keys [op-map] :as data} op-exp]
  (let [op-fn (get op-map (operation-sym op-exp))
        evaled-args (map (partial parse-primitive data)
                         (operation-args op-exp))]
    (apply op-fn evaled-args)))

(comment
  (parse-operation
    {:registry-map {'bar 2 'foo 0} :op-map {'* *}}
    '((op *) (const 3) (reg bar))))

; assign
; ------

(def assign-reg-name second)

(def operation-exp?
  "determines if assign using an `op`"
  (comp (partial tag-of? 'op) #(nth % 2)))

(def assign-operation-exp (partial drop 2))
(comment (assign-operation-exp '(assign foo (op *) (const 3) (const 2))))

(def assign-primitive-exp #(nth % 2))

(operation-exp? '(assign foo (op *) (const 1) (const 2)))

(defn parse-assign [data body]
  (let [reg-name (assign-reg-name body)
        value (if (operation-exp? body)
                (parse-operation data (assign-operation-exp body))
                (parse-primitive data (assign-primitive-exp body)))]
    (-> data
        (assoc-in [:registry-map reg-name] value)
        (update :pc inc))))

(comment
  (let [m {:registry-map {'foo 2 'bar 3} :op-map {'* *} :pc 0}]
    [(parse-assign
       m '(assign foo (const 1)))
     (parse-assign
       m '(assign foo (op *) (const 2) (reg bar)))]))

(def test-condition rest)

; test
; -------------

(defn parse-test [data body]
  (-> data
      (assoc :flag (parse-operation data (test-condition body)))
      (update :pc inc)))

(comment
  (parse-test
    {:registry-map {'bar 2 'foo 0}
     :pc 0
     :op-map {'= =}}
    '(test (op =) (const 3) (reg bar)))
  (parse-test
    {:registry-map {'bar 2 'foo 0}
     :pc 0
     :op-map {'= =}}
    '(test (op =) (const 2) (reg bar))))

; branch
; -------------

(def branch-dest second)
(defn parse-branch [data body]
  (let [dest (parse-primitive data (branch-dest body))]
    (if (:flag data)
      (assoc data :pc dest)
      (update data :pc inc))))

(comment
  (parse-branch
    {:label->idx {'foo 10}
     :flag false
     :pc 0}
    '(branch (label foo)))
  (parse-branch
    {:label->idx {'foo 10}
     :flag true
     :pc 0}
    '(branch (label foo))))

; goto
; -------------
(def goto-dest second)
(defn parse-goto [data body]
  (let [dest (parse-primitive data (goto-dest body))]
    (assoc data :pc dest)))

(comment
  (parse-goto
    {:label->idx {'foo 10}}
    '(goto (label foo)))
  (parse-goto
    {:registry-map {'foo 5}}
    '(goto (reg foo))))

; parse
; -------------

(defn parse-instruction [data instruction]
  (let [type->f {'assign parse-assign
                 'test parse-test
                 'branch parse-branch
                 'goto parse-goto}
        f (or (type->f (tag instruction))
              (throw (Exception. "unexpected instruction")))]
    (f data instruction)))

; run
; -------------

(defn run [registry-map op-map raw-instructions]
  (let [label->idx (extract-label->idx raw-instructions)
        instructions (extract-instructions raw-instructions)
        initial-data {:registry-map registry-map
                      :op-map op-map
                      :stack []
                      :pc 0
                      :flag nil
                      :label->idx label->idx
                      :instructions instructions}]
    (loop [data initial-data]
      (if-let [ins (nth (:instructions data) (:pc data) nil)]
        (recur (parse-instruction data ins))
        data))))

(def default-op-map {'* * '/ /
                     '> > '>= >=
                     '< < '<= <=
                     '+ + '- -
                     '= =})
(comment
  (run
    {'res 1 'counter 4 'base 10}
    default-op-map
    '(
       loop

       (test (op =) (reg counter) (const 0))
       (branch (label done))
       (assign res (op *) (reg base) (reg res))
       (assign counter (op -) (reg counter) (const 1))
       (goto (label loop))

       done)))
