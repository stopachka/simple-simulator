(ns machine-simulator)

(declare make-execution-proc)

; build instructions
; -------------

(def instruction-fn second)
(def body-tag first)

(defn extract-labels [raw-instructions]
      (rest (reduce
              (fn [[idx label->idx instructions] part]
                  (if (symbol? part)
                    [idx
                     (assoc label->idx part (inc idx))
                     instructions]
                    [(inc idx)
                     label->idx
                     (conj instructions part)]))
              [-1 {} []]
              raw-instructions)))

(comment
  (let [[label->idx instructions]
        (extract-labels '((assign foo (const 1))
                          label-one
                          (test (op =) (const 1) (reg foo))
                          (branch (label label-two))
                          label-two
                          (assign bar (const 2))))]
       (println label->idx)
       (println (map-indexed vector instructions))))

(defn assemble-instructions
      "Takes a list of raw instructions
      '(label-one
        (assign foo (const 1))
        (goto label-one))

        and returns two things:
        a. label->idx : a map of labels to their corresponding
          index in the instruction list

          {'label-one 0}

        b. instruction tuples
          [[(assign foo (const 1)) f]
           [(goto (label label-one)) f]]

         f are assembled procedures, that transform a machine
         based on the instructions
      "
      [raw-instructions]
      (let [[label->idx instructions] (extract-labels raw-instructions)]
           [label->idx
            (mapv (fn [body] [body (make-execution-proc body)]) instructions)]))

(comment
  (assemble-instructions
    '((assign foo (const 1))
      label-one
      (test (op =) (const 1) (reg foo))
      (branch (label label-two))
      label-two)))

(defn tag-of? [sym body] (= sym (body-tag body)))

; assign data model
; -------------

(def assign-reg-name
  "(assign foo ...)"
  second)

(comment (assign-reg-name '(assign foo (const 1))))

(def assign-value-exp
  "((op *) (const 1) (reg b) ..."
  (partial drop 2))

(comment (assign-value-exp '(assign foo (op *) (const 1) (const 2))))
(comment (assign-value-exp '(assign foo (const 1))))

(def operation-exp?
  "determines if assign using an `op`"
  (comp (partial tag-of? 'op) #(nth % 2)))

(def assign-operation-exp (partial drop 2))
(comment (assign-operation-exp '(assign foo (op *) (const 3) (const 2))))

(def assign-primitive-exp #(nth % 2))

(operation-exp? '(assign foo (op *) (const 1) (const 2)))

; make expressions
; -------------

(defn make-primitive-proc [prim-exp]
      (fn [{:keys [registry-map label->idx] :as data}]
          (let [res (condp tag-of? prim-exp
                           'const
                           (second prim-exp)
                           'reg
                           (get registry-map (second prim-exp))
                           'label
                           (label->idx (second prim-exp)))]
               res)))

(comment
  (let [f (make-assign-proc '(assign foo (const 3)))]
       (f {:registry-map {'foo 0}
           :op-map {}
           :pc 0}))
  (let [f (make-assign-proc '(assign foo (reg bar)))]
       (f {:registry-map {'foo 0 'bar "hello"}
           :op-map {}
           :pc 0
           :instructions []}))
  (let [f (make-assign-proc '(assign foo (label bar)))]
       (f {:registry-map {'foo 0}
           :op-map {}
           :pc 0
           :instructions '[[nil (assign)] [bar nil]]})))

(def operation-sym (comp second first))
(def operation-args rest)

(defn make-operation-proc [value-exp]
      (let [op-sym (operation-sym value-exp)
            op-arg-fns (map make-primitive-proc (operation-args value-exp))]
           (fn [{:keys [op-map] :as data}]
               (let [op-fn (get op-map op-sym)
                     evaled-args (map (fn [f] (f data)) op-arg-fns)]
                    (apply op-fn evaled-args)))))

(comment
  (let [f (make-assign-proc '(assign foo (op *) (const 3) (reg bar)))]
       (f {:registry-map {'bar 2 'foo 0}
           :pc 0
           :op-map {'* *}
           :instructions []})))

; assign
; -------------
(defn make-assign-proc [body]
      (let [reg-name (assign-reg-name body)
            value-proc (if (operation-exp? body)
                         (make-operation-proc (assign-operation-exp body))
                         (make-primitive-proc (assign-primitive-exp body)))]
           (fn [data]
               (let [new-value (value-proc data)]
                    (-> data
                        (assoc-in [:registry-map reg-name] new-value)
                        (update :pc inc))))))

(def test-condition rest)

; test
; -------------
(defn make-test-proc [body]
      (let [condition-proc (make-operation-proc (test-condition body))]
           (fn [data]
               (-> data
                   (assoc :flag (condition-proc data))
                   (update :pc inc)))))

(comment
  (let [f (make-test-proc '(test (op =) (const 3) (reg bar)))]
       (f {:registry-map {'bar 2 'foo 0}
           :pc 0
           :op-map {'= =}
           :instructions []}))
  (let [f (make-test-proc '(test (op =) (const 3) (reg bar)))]
       (f {:registry-map {'bar 3 'foo 0}
           :pc 0
           :op-map {'= =}
           :instructions []})))

; branch
; -------------
(def branch-dest second)
(defn make-branch-proc [body]
      (let [dest-fn (make-primitive-proc (branch-dest body))]
           (fn [data]
               (if (:flag data)
                 (assoc data :pc (dest-fn data))
                 (update data :pc inc)))))

(comment
  (let [f (make-branch-proc '(branch (label foo)))]
       (f {:registry-map {}
           :pc 0
           :flag false
           :op-map {'= =}
           :instructions '[[nil (assign)] [nil (assign)] [foo nil]]}))
  (let [f (make-branch-proc '(branch (label foo)))]
       (f {:registry-map {'bar 3 'foo 0}
           :pc 0
           :flag true
           :op-map {'= =}
           :instructions '[[nil (assign)] [nil (assign)] [foo nil]]})))

; goto
; -------------
(def goto-dest second)
(defn make-goto-proc [body]
      (let [dest-fn (make-primitive-proc (goto-dest body))]
           (fn [data]
               (assoc data :pc (dest-fn data)))))

(comment
  (let [f (make-goto-proc '(goto (label foo)))]
       (f {:registry-map {}
           :pc 0
           :flag false
           :op-map {'= =}
           :instructions '[[nil (assign)] [nil (assign)] [foo nil]]}))
  (let [f (make-goto-proc '(goto (reg foo)))]
       (f {:registry-map {'foo 3}
           :pc 0
           :flag false
           :op-map {'= =}
           :instructions '[[nil (assign)] [nil (assign)] [foo nil]]})))

; save
; -------------

(def save-reg-name second)
(defn make-save-proc [body]
      (let [reg-name (save-reg-name body)]
           (fn [data]
               (let [reg-v (get-in data [:registry-map reg-name])]
                    (-> data
                        (update :stack conj reg-v)
                        (update :pc inc))))))
(comment
  (let [f (make-save-proc '(save foo))]
       (f {:registry-map {'foo 3}
           :pc 0
           :flag false
           :op-map {'= =}
           :instructions []
           :stack []})))

; restore
; -------------
(def restore-reg-name second)
(defn make-restore-proc [body]
      (let [reg-name (restore-reg-name body)]
           (fn [data]
               (let [v (last (:stack data))]
                    (-> data
                        (update :stack pop)
                        (assoc-in [:registry-map reg-name] v)
                        (update :pc inc))))))
(comment
  (let [f (make-restore-proc '(restore foo))]
       (f {:registry-map {'foo 3}
           :pc 0
           :flag false
           :op-map {'= =}
           :instructions []
           :stack [10]})))

; perform
; -------------
(def perform-operation-exp rest)
(defn make-perform-proc [body]
      (let [value-proc (make-operation-proc (perform-operation-exp body))]
           (fn [data]
               (value-proc data)
               (update data :pc inc))))

(comment
  (let [f (make-perform-proc '(preform (op reset!) (reg foo) (const 1)))]
       (f {:registry-map {'foo (atom nil)}
           :pc 0
           :flag false
           :op-map {'reset! reset!}
           :instructions []
           :stack [10]})))
; analyze
; -------------

(def assign? (partial tag-of? 'assign))
(def test? (partial tag-of? 'test))
(def branch? (partial tag-of? 'branch))
(def goto? (partial tag-of? 'goto))
(def save? (partial tag-of? 'save))
(def restore? (partial tag-of? 'restore))
(def perform? (partial tag-of? 'perform))

(defn make-execution-proc [body]
      (cond
        (assign? body)
        (make-assign-proc body)

        (test? body)
        (make-test-proc body)

        (branch? body)
        (make-branch-proc body)

        (goto? body)
        (make-goto-proc body)

        (save? body)
        (make-save-proc body)

        (restore? body)
        (make-restore-proc body)

        (perform? body)
        (make-perform-proc body)

        :else
        (throw (Exception. (format "Unsupported instruction label %s" body)))))


; run
; -------------

(defn run [registry-map op-map raw-instructions]
      (let [[label->idx instructions] (assemble-instructions raw-instructions)
            initial-data {:registry-map registry-map
                          :op-map op-map
                          :stack []
                          :pc 0
                          :flag nil
                          :label->idx label->idx
                          :instructions instructions}]
           (loop [data initial-data]
                 (if-let [f (instruction-fn
                              (nth (:instructions data) (:pc data) nil))]
                         (recur (f data))
                         data))))

(def default-op-map {'* * '/ /
                     '> > '>= >=
                     '< < '<= <=
                     '+ + '- -
                     '= =})
(comment
  (run
    {'res 1 'counter 3 'base 10}
    default-op-map
    '(
       loop

       (test (op =) (reg counter) (const 0))
       (branch (label done))
       (assign res (op *) (reg base) (reg res))
       (assign counter (op -) (reg counter) (const 1))
       (goto (label loop))

       done)))
