(require 'cl)
(load-file "C:/Program Files/Spacemacs/.emacs.d/elpa/dash-20180413.30/dash.el")
(require 'dash)
(require 'seq)

;; create-node-list
;; create a list of nodes using the adjacencies list
(defun create-node-list (adjacencies)
  (mapcar 'first adjacencies)
)

;; create-edge-list
;; using the adjacencies list create a list of edges
(defun create-edge-list (adjacencies)
  ;; remove duplicates from the list
  (remove-duplicates 
    ;; create a list of all edges as defined in the adjacencies list
    (remove-if 'null (apply 'append (mapcar (lambda (lis) (mapcar (lambda (adj) (if (not (= (car lis) adj)) (cons (car lis) adj))) lis)) adjacencies)))

    ;; is a duplicate test
    :test (lambda (left right) 
                (if (or (and (= (car left) (car right)) (= (cdr left) (cdr right)))
                        (and (= (cdr left) (car right)) (= (car left) (cdr right)))) 
                    t
                     nil)))
)

;; edge-in-path
;; determine if the edge is in the path already
(defun edge-in-path (edge path)
  (or (seq-contains path edge) (seq-contains path (reverse edge)))
)

;; get-edges-containing-node
;; returns a list of edges that contain a given node
(defun get-edges-containing-node (node edge-list)
  (remove-if 'null (mapcar (lambda (edge) (if (seq-contains edge node) edge)) edge-list))
)

;; get outgoing edges from a node
(defun outgoing (node edges)
  (remove-if (lambda (edge) (if (or (= node (car edge)) (= node (cdr edge))) nil t)) edges))

;; remove an edge from the list of edges
(defun remove-edge (edge edges)
  (remove-if (lambda (e) (if (or (and (= (car edge) (car e)) (= (cdr edge) (cdr e)))
                                 (and (= (car edge) (cdr e)) (= (cdr edge) (car e)))) t)) edges))

;; create-circuit
;; create a circuit from the adjacencies list
(defun walk (edges node)
  (let  ((path (list node))
          (adj-nodes (outgoing node edges))
          (e nil))
         (while (not (null adj-nodes))
           (setq e (first adj-nodes)
                 e (if (= node (car e)) e (cons (cdr e) (car e)))
                 edges (remove-edge e edges)
                 path  (append path (list (cdr e)))
                 node (cdr e)
                 adj-nodes (outgoing node edges)))
         (list path edges)
   )
)

;;insert-circuit
;; inserts a circuit into another circuit
(defun insert-circuit (circuit circuit-to-add)
  (let* ((common-nodes (remove-duplicates (intersection circuit circuit-to-add)))
         (common-node (if (plusp (length common-nodes))
                          (car common-nodes)
                        (error "no nodes in common between %s and $s" circuit circuit-to-add)))
         (position-of-common-node-in-circuit (loop for node in circuit for i from 0 when (= common-node node) return i))
         (position-of-common-node-in-circuit-to-add (loop for node in circuit-to-add for i from 0 when (= common-node node) return i))
         (up-to-common-node-in-circuit (subseq circuit 0 position-of-common-node-in-circuit))
         (up-to-common-node-in-circuit-to-add (subseq circuit-to-add 0 position-of-common-node-in-circuit-to-add))
         (after-common-node-in-circuit (subseq circuit position-of-common-node-in-circuit))
         (after-common-node-in-circuit-to-add (subseq circuit-to-add position-of-common-node-in-circuit-to-add)))
        (append up-to-common-node-in-circuit
                after-common-node-in-circuit-to-add
           (if (and (plusp (length up-to-common-node-in-circuit-to-add))
                    (= (car up-to-common-node-in-circuit-to-add)
                       (car (last after-common-node-in-circuit-to-add))))
               (cdr up-to-common-node-in-circuit-to-add)
             up-to-common-node-in-circuit-to-add)
           (if (and (plusp (length after-common-node-in-circuit))
                    (= (car after-common-node-in-circuit)
                       (car (last after-common-node-in-circuit-to-add))))
               (cdr after-common-node-in-circuit)
             after-common-node-in-circuit))))

;; pick-random-node
;; picks a random node from the list to start with
(defun pick-random-node (nodes)
  (nth (random (length nodes)) nodes))

;; hierolzer-algorithm
;; Finds Eulerian Circuits from a list of adjacencies
(defun hierolzer-algorithm (adjacencies) 
  ;; ensure a list was passed
  (if (not (listp adjacencies))
      (user-error "Error: List expected as argument"))

  ;; ensure the list has values
  (if (= (length adjacencies) 0)
      (user-error "Error: List contains no values"))

  ;; ensure that adjacencies is a list of lists
  (if (not (every 'listp adjacencies))
      (user-error "Error: Not all elements of the list are lists"))

  ;; ensure all lists in adjaceny list have length greater than 1
  (if (not (every (lambda (lis) (<= 2 (length lis))) adjacencies))
      (user-error "Error: Not all nodes have adjacencies"))

  ;; If the table comes from a table like this:
  ;; | 1 | 2 4 6 6 |
  ;; ....
  ;; then convert it into a list without strings
  (if (-any (lambda (lis) (stringp (cadr lis))) adjacencies)
      (setq adjacencies 
        (mapcar (lambda (lis) (apply 'list (car lis) (mapcar 'string-to-number (split-string (cadr lis) " ")))) adjacencies)))

  ;; Check that all nodes have an even degree
  (if (-any 'oddp (mapcar (lambda (lis) (length (cdr lis))) adjacencies))
      (user-error "Not all nodes have an even degree"))

  ;; create the circuit
  (let (node-list edge-list curr-node path circuit circuit-to-add)
       (setq node-list (create-node-list adjacencies)
             edge-list (create-edge-list adjacencies)
             curr-node (pick-random-node node-list)
             path (walk edge-list curr-node)
             circuit (first path)
             not-visited (second path)
             circuit-to-add nil)
       (while (plusp (length not-visited))
              (setq curr-node (car (first not-visited))
                    path (walk edge-list curr-node)
                    circuit-to-add (first path)
                    not-visited (second path)
                    circuit (insert-circuit circuit circuit-to-add)))
       (if (= (1- (length circuit)) (length edge-list)) circuit (hierolzer-algorithm adjacencies))) 
)

(provide 'hierolzer)
