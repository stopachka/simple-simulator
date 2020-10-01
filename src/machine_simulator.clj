(ns machine-simulator)

; factorial-instructions
; ----------------------

(def factorial-instructions
  '(
    (assign counter (const 1))
    (assign res (const 1))

    loop

    (test (op >) (reg counter) (reg n))
    (branch (label done))

    (assign res (op *) (reg counter) (reg res))
    (assign counter (op +) (reg counter) (const 1))
    (goto (label loop))

    done))

; misc
; ----

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
  (extract-label->idx factorial-instructions)
  (map-indexed vector (extract-instructions factorial-instructions)))

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
  (parse-primitive {} '(const 3))
  (parse-primitive {:registry-map {'foo 3}} '(reg foo))
  (parse-primitive {:label->idx {'label-one 3}} '(label label-one)))

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

(defn exec-assign [data body]
  (let [reg-name (assign-reg-name body)
        value (if (operation-exp? body)
                (parse-operation data (assign-operation-exp body))
                (parse-primitive data (assign-primitive-exp body)))]
    (-> data
        (assoc-in [:registry-map reg-name] value)
        (update :pc inc))))

(comment
  (let [m {:registry-map {'foo 2 'bar 3} :op-map {'* *} :pc 0}]
    [(exec-assign
       m '(assign foo (const 1)))
     (exec-assign
       m '(assign foo (op *) (const 2) (reg bar)))]))

; test
; -------------

(def test-condition rest)

(defn exec-test [data body]
  (-> data
      (assoc :flag (parse-operation data (test-condition body)))
      (update :pc inc)))

(comment
  (exec-test
    {:registry-map {'bar 2 'foo 0}
     :pc 0
     :op-map {'= =}}
    '(test (op =) (const 3) (reg bar)))
  (exec-test
    {:registry-map {'bar 2 'foo 0}
     :pc 0
     :op-map {'= =}}
    '(test (op =) (const 2) (reg bar))))

; branch
; -------------

(def branch-dest second)
(defn exec-branch [data body]
  (let [dest (parse-primitive data (branch-dest body))]
    (if (:flag data)
      (assoc data :pc dest)
      (update data :pc inc))))

(comment
  (exec-branch
    {:label->idx {'foo 10}
     :flag false
     :pc 0}
    '(branch (label foo)))
  (exec-branch
    {:label->idx {'foo 10}
     :flag true
     :pc 0}
    '(branch (label foo))))

; goto
; -------------
(def goto-dest second)

(defn exec-goto [data body]
  (let [dest (parse-primitive data (goto-dest body))]
    (assoc data :pc dest)))

(comment
  (exec-goto {:label->idx {'foo 10}} '(goto (label foo)))
  (exec-goto {:registry-map {'foo 5}} '(goto (reg foo))))

; parse
; -------------

(defn exec-instruction [data instruction]
  (let [type->f {'assign exec-assign
                 'test exec-test
                 'branch exec-branch
                 'goto exec-goto}
        f (or (type->f (tag instruction))
              (throw (Exception. "unexpected instruction")))]
    (f data instruction)))

; run
; -------------

(defn run [registry-map op-map raw-instructions]
  (let [label->idx (extract-label->idx raw-instructions)
        instructions (extract-instructions raw-instructions)
        initial-machine-state {:registry-map registry-map
                               :op-map op-map
                               :stack []
                               :pc 0
                               :flag nil
                               :label->idx label->idx}]
    (loop [machine-state initial-machine-state]
      (println (nth instructions (:pc machine-state) nil))
      (if-let [ins (nth instructions (:pc machine-state) nil)]
        (recur (exec-instruction machine-state ins))
        machine-state))))

(def default-op-map {'* * '/ /
                     '> > '>= >=
                     '< < '<= <=
                     '+ + '- -
                     '= =})
(comment
  (run
    {'n 10 'counter nil 'res nil}
    default-op-map
    factorial-instructions))
