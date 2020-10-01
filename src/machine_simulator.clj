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

; instructions
; -------------

(def tag first)
(def drop-tag rest)
(defn tag-of? [sym s] (= sym (tag s)))

(defn extract-label->idx [raw-instructions]
  (second
    (reduce
      (fn [[idx label->idx] part]
        (if (symbol? part)
          [idx (assoc label->idx part idx)]
          [(inc idx) label->idx]))
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
(def assign-operator #(nth % 2))

(def operation-exp?
  (comp (partial tag-of? 'op) assign-operator))

(def assign-operation-exp (partial drop 2))

(defn exec-assign
  "Assign comes in two forms:

  (assign reg-name <primitive-op>)
  i.e (assign foo (const 1))

  (assign reg-name <operation> <args...>)
  i.e (assign foo (op *) (const 2) (reg foo))"
  [data ins]
  (let [reg-name (assign-reg-name ins)
        value (if (operation-exp? ins)
                (parse-operation data (assign-operation-exp ins))
                (parse-primitive data (assign-operator ins)))]
    (-> data
        (assoc-in [:registry-map reg-name] value)
        (update :pc inc))))

(comment
  (def m {:registry-map {'foo 2 'bar 3} :op-map {'+ +} :pc 0})
  (exec-assign m '(assign foo (const 3)))
  (exec-assign m '(assign foo (op +) (const 1) (reg foo))))

; test
; -------------

(defn exec-test [data ins]
  (-> data
      (assoc :flag (parse-operation data (drop-tag ins)))
      (update :pc inc)))

(comment
  (exec-test
    {:registry-map {'bar 2 'foo 0} :pc 0 :op-map {'= =}}
    '(test (op =) (const 3) (reg bar)))
  (exec-test
    {:registry-map {'bar 2 'foo 0} :pc 0 :op-map {'= =}}
    '(test (op =) (const 2) (reg bar))))

; branch
; -------------

(def branch-dest second)
(defn exec-branch [data ins]
  (let [dest (parse-primitive data (branch-dest ins))]
    (if (:flag data)
      (assoc data :pc dest)
      (update data :pc inc))))

(comment
  (exec-branch
    {:label->idx {'foo 10} :flag false :pc 0}
    '(branch (label foo)))
  (exec-branch
    {:label->idx {'foo 10} :flag true :pc 0}
    '(branch (label foo))))

; goto
; -------------

(def goto-dest second)

(defn exec-goto [data ins]
  (assoc data :pc (parse-primitive data (goto-dest ins))))

(comment
  (exec-goto {:label->idx {'foo 10}} '(goto (label foo)))
  (exec-goto {:registry-map {'foo 5}} '(goto (reg foo))))

; parse
; -------------

(defn exec-instruction [data ins]
  (let [type->f {'assign exec-assign
                 'test exec-test
                 'branch exec-branch
                 'goto exec-goto}
        f (or (type->f (tag ins))
              (throw (Exception. "Unexpected instruction")))]
    (f data ins)))

; run
; -------------

(defn run [registry-map op-map raw-instructions]
  (let [label->idx (extract-label->idx raw-instructions)
        instructions (extract-instructions raw-instructions)
        initial-machine-state {:registry-map registry-map
                               :op-map op-map
                               :pc 0
                               :flag nil
                               :label->idx label->idx}]
    (loop [machine-state initial-machine-state]
      (if-let [ins (nth instructions (:pc machine-state) nil)]
        (recur (exec-instruction machine-state ins))
        machine-state))))

(comment
  (run
    {'n 5 'counter nil 'res nil}
    {'* * '> > '+ +}
    factorial-instructions))
