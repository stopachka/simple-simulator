# simple-simulator

Simple machine simulator: a complement to [this essay.](https://stopa.io/post/255)

```bash
clj
```

```clojure
(require 'machine-simulator)
(in-ns 'machine-simulator) 
(get-in
  (run
    {'n 5 'counter 1 'res 1}
    {'* * '> > '+ +}
    factorial-instructions)
  [:registry-map 'res])
```
