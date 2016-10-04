(ns Player
  (:gen-class))

(def PI10 (/ Math/PI 10.0))
(def PIPI (* Math/PI 2.0))
(def PI Math/PI)

(defn debug-value [x]
  (binding [*out* *err*]
    (println x))
  x)

(defn read-checkpoints
  ([]
   (let [laps (read)
         checkpointCount (read)
         checkpoints (read-checkpoints checkpointCount)]
     checkpoints))
  ([checkpointCount] (read-checkpoints checkpointCount []))
  ([checkpointCount checkpoints]
   (if (> checkpointCount 0)
     (let [checkpointX (read)
           checkpointY (read)]
       (recur (dec checkpointCount)
              (conj checkpoints [checkpointX checkpointY])))
     checkpoints)))

(defn read-pod []
  (let [x (read) y (read)
        vx (read) vy (read)
        angle (read)
        nextCheckPointId (read)]
    {:pos [x y]
     :speed [vx vy]
     :angle angle
     :next nextCheckPointId}))

(defn limit-to-180 [x]
  (if (< x -180.0)
    (+ x 360.0)
    (if (> x 180.0)
      (- x 360.0)
      x)))

(defn fix-angle-degrees [angle]
  (if (< angle 0) (+ 360 angle)
      (if (> angle 360) (- angle 360) angle)))

(defn fix-angle-radians [angle]
  (if (< angle 0) (+ PIPI angle)
      (if (> angle PIPI) (- angle PIPI) angle)))

(defn rad2deg [angle]
  (fix-angle-degrees (* angle (/ 180.0 PI))))

(defn deg2rad [angle]
  (let [radians (* angle (/ PI 180.0))]
    (fix-angle-radians radians)))

(defn angle-degrees [[x1 y1] [x2 y2]]
  (let [angle (Math/atan2 (- y2 y1) (- x2 x1))
        degrees (rad2deg angle)]
     degrees))
(defn vector-angle-degrees [[x y]]
  (let [angle (Math/atan2 y x)
        degrees (rad2deg angle)]
     degrees))

(defn angle-radians [[x1 y1] [x2 y2]]
  (let [angle (Math/atan2 (- y2 y1) (- x2 x1))
        positive (fix-angle-radians angle)]
    positive))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))
(def dist manhattan-distance)

(defn norm [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn normalize [[x y :as v]]
  (let [n (norm v)]
    [(/ x n) (/ y n)]))

(defn vec-+ [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn vec-* [[x y] value]
  [(* x value) (* y value)])

(defn vec-floor [[x y]]
  [(int x) (int y)])

(defn vec-round [[x y]]
  [(if (integer? x) x (Math/round x))
   (if (integer? y) y (Math/round y))])

(defn vec-from-points [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)])

(defn dot-product [[x1 y1 :as a] [x2 y2 :as b]]
  (let [angle (Math/atan2 (- y2 y1) (- x2 x1))]
    (* (norm a) (norm b) (Math/cos angle))))

(defn vec-rotate [[x y] angle-rad]
  (let [cs (Math/cos angle-rad)
        sn (Math/sin angle-rad)
        new-x (- (* x cs) (* y sn))
        new-y (+ (* x sn) (* y cs))]
    [new-x new-y]))

(defn points-distance [p1 p2]
  (let [v (vec-from-points p1 p2)
        n (norm v)]
    n))

(defn analyze-state [checkpoints pods state]
  (let [
        target (get checkpoints (:next state))
        next-target-id (mod (inc (:next state)) (count checkpoints))
        next-target (get checkpoints next-target-id)

        ideal-angle (angle-degrees next-target target)
        speed-angle (vector-angle-degrees (:speed state))

        pod-target-vector (vec-from-points (:pos state) target)
        
        angle-to-target (angle-degrees target (:pos state))
        diff-angle-speed (Math/abs (- (vector-angle-degrees (:speed state)) angle-to-target))

        diff-angle-speed-ideal (Math/abs (limit-to-180 (- ideal-angle)))

        distance-to-checkpoint (points-distance (:pos state) target)
        distance-to-checkpoint2 (points-distance (vec-+ (:pos state) (:speed state)) target)
        rating (if (> 2000000 distance-to-checkpoint)
                 (+ distance-to-checkpoint2 (* 10 diff-angle-speed-ideal))
                 distance-to-checkpoint2)
        ]
    (assoc state
           ;:target target
           ;:next-target next-target
           ;:ideal-angle ideal-angle
           ;:pod-target-vector pod-target-vector
           ;:angle-to-target angle-to-target
           ;:diff-angle-speed diff-angle-speed
           :distance-to-checkpoint distance-to-checkpoint
           :distance-to-checkpoint2 distance-to-checkpoint2
           :rating rating
           )))

(defn next-state [checkpoints pods state target thrust]
  (let [[x y] (:pos state)
        [vx vy] (:speed state)
        angle (:angle state)
        angle-to-target (angle-degrees (:pos state) target)
        asked-turn (let [x (- angle-to-target angle)
                         x2 (if (< x -180.0)
                              (+ x 360.0)
                              (if (> x 180.0)
                                (- x 360.0)
                                x))] x2)
        turn (min 18.0 (max (- 18.0) asked-turn))
        i-velocity-x (+ vx (* thrust (Math/cos (deg2rad (+ angle turn)))))
        i-velocity-y (+ vy (* thrust (Math/sin (deg2rad (+ angle turn)))))
        new-x (Math/round (+ x i-velocity-x))
        new-y (Math/round (+ y i-velocity-y))
        new-vx (int (* 0.85 i-velocity-x))
        new-vy (int (* 0.85 i-velocity-y))
        new-angle (mod (Math/round (+ angle turn)) 360)
        checkpoint-reached? (> 600 (norm (vec-from-points [new-x new-y] (get checkpoints (:next state)))))
        next-checkpoint-id (if checkpoint-reached? (mod (inc (:next state)) (count checkpoints)) (:next state))
        ]
    (analyze-state checkpoints
                   pods
                   (assoc state
                          :pos [new-x new-y]
                          :angle new-angle
                          :speed [new-vx new-vy]
                          :next next-checkpoint-id
                          :params [target thrust]))))

(defn simulate-one-turn [checkpoints pods state heuristic]
  (let [[target thrust] (heuristic checkpoints pods state)
        new-state (next-state checkpoints pods state target thrust)]
    new-state))

(defn optimize-target [checkpoints pods state heuristic]
  (let [position (:pos state)
        [target thrust] (heuristic checkpoints pods state)
        straight-state (next-state checkpoints pods state target thrust)
        pod-to-target (vec-from-points position target)
        rotated-ccw (vec-rotate pod-to-target (- PI10))
        rotated-cw (vec-rotate pod-to-target PI10)
        target-ccw (vec-+ position rotated-ccw)
        [_ thrust-ccw] (heuristic checkpoints pods state target-ccw)
        target-cw (vec-+ position rotated-cw)
        [_ thrust-cw] (heuristic checkpoints pods state target-cw)
        ccw-state (next-state checkpoints pods state target-ccw thrust-ccw)
        cw-state (next-state checkpoints pods state target-cw thrust-cw)
        dists (map (partial points-distance position) (map :pos [straight-state ccw-state cw-state]))
        sorted-dists (sort dists)
        final-target (vec-round (cond (= (first sorted-dists) (first dists)) target
                                      (= (first sorted-dists) (second dists)) target-ccw
                                      :default target-cw))
        ]
    [final-target thrust]))

; compute next state given target and thrust
; compute score of current state
; imagine different "random" ways to next turn
; repeat a number of times and keep the best final score

(defn thrust-from-target [checkpoints pods pod target]
  (let [
        angle-to-target (angle-degrees (:pos pod) target)
        diff-angle (Math/abs (limit-to-180 (- (:angle pod) angle-to-target)))
        msa 90.0
        thrust (if (= (:speed pod) [0 0])
                  200
                  (max 0 (min 200 (int (* 200 (/ (- msa (min msa diff-angle)) msa))))))
        ]
    thrust))

(defn max-speed-to-target-heuristic [checkpoints pods pod]
  (let [
        target (get checkpoints (:next pod))
        angle-to-target (angle-degrees (:pos pod) target)
        diff-angle (Math/abs (- (:angle pod) angle-to-target))
        diff-angle-speed (Math/abs (- (vector-angle-degrees (:speed pod)) angle-to-target))
        thrust (if (= (:speed pod) [0 0])
                  200
                  (max 0 (min 200 (int (* 200 (/ (- 90.0 (min 90.0 diff-angle)) 90.0))))))
        ]
    [target thrust]))

(defn anticipate-next-checkpoint-heuristic [checkpoints pods pod]
  (let [
        next-target (get checkpoints (:next pod))
        next-next-target (get checkpoints (mod (inc (:next pod)) (count checkpoints)))
        pos (:pos pod)
        speed (:speed pod)
        angle (:angle pod)
        extrapolated-position (vec-+ pos speed)
        reaching-goal-soon? (> 600 (points-distance extrapolated-position next-target))
        target (if reaching-goal-soon? next-next-target next-target)

        angle-to-target (angle-degrees pos target)
        speed-angle (vector-angle-degrees speed)
        diff-angle (limit-to-180 (- angle-to-target speed-angle))
        angle-correction (if (and (> 90.0 (Math/abs diff-angle))
                                  (> (norm speed) 100.0))
                           (min 18.0 (max -18.0 diff-angle))
                           0.0)
        new-target (vec-+ pos (vec-round (vec-rotate (vec-from-points pos target) (deg2rad angle-correction))))
        thrust (thrust-from-target checkpoints pods pod new-target)
        ]
    (debug-value [angle-to-target speed-angle diff-angle angle-correction])
    [new-target thrust]))

(defn try-multiple-angles-heuristic [checkpoints pods pod]
  (let [pos (:pos pod)
        angle-pod (:angle pod)
        speed (:speed pod)
        next-checkpoint (get checkpoints (:next pod))
        angle-to-checkpoint (angle-degrees pos next-checkpoint)
        distance-to-checkpoint (points-distance pos next-checkpoint)
        angle (if (= angle-pod -1) angle-to-checkpoint angle-pod)
        
        ;diff-angle (limit-to-180 (- angle-to-checkpoint angle))

        possible-angles (map fix-angle-degrees (reductions + (- angle 18) (repeat 36 1)))
        possible-targets (map #(vec-round (vec-+ pos (vec-* [(Math/cos (deg2rad %))
                                                             (Math/sin (deg2rad %))] 10000)))
                              possible-angles)
        possible-thrusts (let [thrusts (iterate (partial + 25) 0)]
                           (if (and false
                                    (> (norm speed) 300)
                                    (< distance-to-checkpoint 2000))
                             (take 3 thrusts)
                             (take 9 thrusts)))
        states (for [target possible-targets
                     thrust possible-thrusts]
                 (next-state checkpoints pods pod target thrust))
        sorted-states (sort-by :distance-to-checkpoint2 states)
        best-state (first sorted-states)
        ;best-state (first states)
        ;debug-angles (map #(Math/round (angle-degrees pos %)) possible-targets)
        ]
    ;(debug-value angle)
    ;(debug-value possible-angles)
    ;(debug-value debug-angles)
    ;(doseq [s sorted-states] (debug-value s))
    (:params best-state)))

(defn try-multiple-angles-heuristic2 [checkpoints pods pod]
  (let [pos (:pos pod)
        angle-pod (:angle pod)
        speed (:speed pod)
        next-checkpoint (get checkpoints (:next pod))
        angle-to-checkpoint (angle-degrees pos next-checkpoint)
        distance-to-checkpoint (points-distance pos next-checkpoint)
        angle (if (= angle-pod -1) angle-to-checkpoint angle-pod)
        
        ;diff-angle (limit-to-180 (- angle-to-checkpoint angle))

        possible-angles (map fix-angle-degrees (reductions + (- angle 18) (repeat 36 1)))
        possible-targets (map #(vec-round (vec-+ pos (vec-* [(Math/cos (deg2rad %))
                                                             (Math/sin (deg2rad %))] 10000)))
                              possible-angles)
        possible-thrusts (let [thrusts (iterate (partial + 25) 0)]
                           (if (and ;false
                                    (> (norm speed) 300)
                                    (< distance-to-checkpoint 2000))
                             (take 3 thrusts)
                             (take 9 thrusts)))
        states (for [target possible-targets
                     thrust possible-thrusts]
                 (next-state checkpoints pods pod target thrust))
        sorted-states (sort-by :rating states)
        best-state (first sorted-states)
        ;best-state (first states)
        ;debug-angles (map #(Math/round (angle-degrees pos %)) possible-targets)
        ]
    ;(debug-value angle)
    ;(debug-value possible-angles)
    ;(debug-value debug-angles)
    ;(doseq [s (sort-by :distance-to-checkpoint states)] (debug-value s))
    (:params best-state)))


(defn -main [& args]
    (let [checkpoints (read-checkpoints)]
      (while true
        (loop [states []]
          (let [pods [(read-pod) (read-pod) (read-pod) (read-pod)]
                [target1 thrust1] (try-multiple-angles-heuristic checkpoints pods (first pods))
                [target2 thrust2] (try-multiple-angles-heuristic2 checkpoints pods (second pods))
                next-state1 (next-state checkpoints pods (first pods) target1 thrust1)
                next-state2 (next-state checkpoints pods (second pods) target2 thrust2)
                pod3 (get pods 2)
                pod4 (get pods 3)
                opponents-next-positions (map vec-+
                                              (map :pos [pod3 pod4])
                                              (map :speed [pod3 pod4]))
                shield1 (some (partial > 800) (map points-distance
                                                   (repeat (:pos next-state1))
                                                   opponents-next-positions))
                shield2 false; (some (partial > 800) (map points-distance
                              ;                     (repeat (:pos next-state2))
                               ;                    opponents-next-positions))
                ]
            
            ;(debug-value pods)
            ;(debug-value next-state1)
            ;(debug-value next-state11)
            ;(debug-value [(:pos (second pods)) (:pos (first states))])
            ;(debug-value [(:angle (second pods)) (:angle (first states))])
            (println (first target1) (second target1) (if shield1 "SHIELD SHIELD" thrust1))
            (println (first target2) (second target2) (if shield2 "SHIELD SHIELD" thrust2))
            (recur (cons next-state1 states)))))))
