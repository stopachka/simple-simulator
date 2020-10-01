(ns machine-simulator)

; factorial
; ---------------

(defn factorial [n]
  (loop [res 1
         counter 1]
    (if (< n counter)
      res
      (recur
        (* counter res)
        (inc counter)))))

(def factorial-instructions
  '(
     start

     (test (op >) (reg counter) (reg n))
     (branch (label done))

     (assign res (op *) (reg counter) (reg res))
     (assign counter (op +) (reg counter) (const 1))
     (goto (label start))

     done))

; parse-primitive
; ---------------

(def ex-machine-state-v0
  {:registry-map {'n 10 'res 1 'counter 1}
   :label->idx {'start 0 'done 5}})

(def tag first)
(defn tag-of? [sym s] (= sym (tag s)))

(defn parse-primitive [{:keys [registry-map label->idx] :as machine-state}
                       prim-exp]
  (condp tag-of? prim-exp
    'const
    (second prim-exp)
    'reg
    (get registry-map (second prim-exp))
    'label
    (label->idx (second prim-exp))))

(comment
  (parse-primitive ex-machine-state-v0 '(const 1))
  (parse-primitive ex-machine-state-v0 '(reg n))
  (parse-primitive ex-machine-state-v0 '(label done)))

; parse-operation
; ---------------

(def ex-machine-state-v1
  {:registry-map {'n 10 'res 1 'counter 1}
   :label->idx {'start 0 'done 5}
   :op-map {'* * '+ + '> >}})

(def operation-sym (comp second first))
(def operation-args rest)

(defn parse-operation [{:keys [op-map] :as data} op-exp]
  (let [op-fn (get op-map (operation-sym op-exp))
        evaled-args (map (partial parse-primitive data)
                         (operation-args op-exp))]
    (apply op-fn evaled-args)))

(comment
  (parse-operation ex-machine-state-v1 '((op >) (reg counter) (reg n)))
  (parse-operation ex-machine-state-v1 '((op *) (reg counter) (reg res)))
  (parse-operation ex-machine-state-v1 '((op +) (reg counter) (const 1))))

; assign
; ------

(def ex-machine-state-v2
  {:registry-map {'n 10 'res 1 'counter 1}
   :label->idx {'start 0 'done 5}
   :op-map {'* * '+ + '> >}
   :idx 0})

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
        val (if (operation-exp? ins)
              (parse-operation data (assign-operation-exp ins))
              (parse-primitive data (assign-operator ins)))]
    (-> data
        (assoc-in [:registry-map reg-name] val)
        (update :idx inc))))

(comment
  (select-keys (exec-assign ex-machine-state-v2 '(assign counter (const 10)))
               [:registry-map :idx])
  (select-keys (exec-assign ex-machine-state-v2 '(assign counter (op +) (reg counter) (const 1)))
               [:registry-map :idx]))

; goto
; -------------

(def goto-dest second)

(defn exec-goto [data ins]
  (assoc data :idx (parse-primitive data (goto-dest ins))))

(comment
  (select-keys (exec-goto ex-machine-state-v2 '(goto (label done)))
               [:label->idx :idx]))

; test
; ----

(def ex-machine-state-v3
  {:registry-map {'n 10 'res 1 'counter 1}
   :label->idx {'start 0 'done 5}
   :op-map {'* * '+ + '> >}
   :idx 0
   :test-passed? false})

(def drop-tag rest)

(defn exec-test [data ins]
  (-> data
      (assoc :test-passed? (parse-operation data (drop-tag ins)))
      (update :idx inc)))

(comment
  (:test-passed? (exec-test ex-machine-state-v3 '(test (op >) (reg counter) (reg n))))
  (:test-passed? (exec-test ex-machine-state-v3 '(test (op >) (reg n) (reg counter)))))

; branch
; -------

(def branch-dest second)
(defn exec-branch [data ins]
  (let [dest (parse-primitive data (branch-dest ins))]
    (if (:test-passed? data)
      (assoc data :idx dest)
      (update data :idx inc))))

(comment
  (exec-branch
    {:label->idx {'done 5} :test-passed? false :idx 0}
    '(branch (label done)))
  (exec-branch
    {:label->idx {'done 5} :test-passed? true :idx 0}
    '(branch (label done))))


; exec
; -------------

(defn exec-ins [data ins]
  (let [type->f {'assign exec-assign
                 'test exec-test
                 'branch exec-branch
                 'goto exec-goto}
        f (or (type->f (tag ins))
              (throw (Exception. "Unexpected instruction")))]
    (f data ins)))

(comment
  (:registry-map (exec-ins ex-machine-state-v3 '(assign counter (const 5)))))

; instructions
; -------------

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
  (vec (remove symbol? raw-instructions)))

(comment
  (extract-label->idx factorial-instructions)
  (extract-instructions factorial-instructions))

; run
; -------------

(defn run [registry-map op-map raw-instructions]
  (let [label->idx (extract-label->idx raw-instructions)
        instructions (extract-instructions raw-instructions)
        initial-machine-state {:registry-map registry-map
                               :op-map op-map
                               :idx 0
                               :test-passed? nil
                               :label->idx label->idx}]
    (loop [machine-state initial-machine-state]
      (if-let [ins (nth instructions (:idx machine-state) nil)]
        (recur (exec-ins machine-state ins))
        machine-state))))

(comment
  (get-in
    (run
      {'n 5 'counter 1 'res 1}
      {'* * '> > '+ +}
      factorial-instructions)
    [:registry-map 'res]))
