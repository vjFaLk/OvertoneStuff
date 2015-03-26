(ns tutorial.core
  (:use overtone.core))

(boot-external-server)


(use 'overtone.inst.sampled-piano)
(use 'overtone.synth.stringed)
(use 'overtone.synth.retro)


(defn play-chord [a-chord]
  (doseq [note a-chord] (sampled-piano note)))




(let [time (now)]
  (at time (play-chord (chord :D3 :major7)))
  (at (+ 2000 time) (play-chord (chord :A3 :major)))
  (at (+ 3000 time) (play-chord (chord :A3 :major7)))
  (at (+ 4300 time) (play-chord (chord :F3 :major7))))

(defn play-progression [chords]
  (if (empty? chords) nil
    (doseq []
      (play-chord (first chords))
      (Thread/sleep 2000)
      (play-progression (rest chords)))))

(defn play
  [time notes sep]
  (let [note (first notes)]
    (when note
      (at time (sampled-piano note)))
    (let [next-time (+ time sep)]
      (apply-by next-time play [next-time (rest notes) sep]))))

(def many-notes [:G3 :G3 :A3 nil :G3 nil :C4 nil :B3
                  nil nil :G3 :G3 :A3 nil :G3 nil :D4 nil :C4
                  nil nil :G3 :G3 nil :G4 nil :E4 nil :C4 nil :B3 nil :A3
                  nil  :F4 :F4 :E4 nil :C4 nil :D4 nil :C4])

;Happy Birthday!
(play (now) (map note many-notes) 100)
(play (now) (map note many-notes) 101)
(play (now) (map note many-notes) 200)

(stop)

(play-progression (play-chord(chord :D3 :major7)))


(definst beep [note 60]
  (let [sound-src (sin-osc (midicps note))
        env       (env-gen (perc 0.01 1.0) :action FREE)] ; sam uses :free
    (* sound-src env)))

(beep)

(for [i (range 110)] (at (+ (now) (* i 20)) (beep i)))


(definst plucked-string [note 60 amp 0.8 dur 2 decay 30 coef 0.3 gate 1]
  (let [freq   (midicps note)
        noize  (* 0.8 (white-noise))
        dly    (/ 1.0 freq)
        plk    (pluck noize gate dly dly decay coef)
        dist   (distort plk)
        filt   (rlpf dist (* 12 freq) 0.6)
        clp    (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (* amp (env-gen (perc 0.0001 dur)) reverb)))


(plucked-string)


;; note: the underscores are rests
(def reich-degrees [:vi :vii :i+ :_ :i+ :i+ :i+ :vii :vi :_ :vii :_ :vi :_ :vii])
(def pitches (degrees->pitches reich-degrees :diatonic :C4))



(defn play
  [time notes sep]
  (let [note (first notes)]
    (when note
      (at time (plucked-string note)))
    (let [next-time (+ time sep)]
      (apply-by next-time play [next-time (rest notes) sep]))))

(play (now) pitches 300)


(let [t (+ 500 (now))]
  (play t (cycle pitches) 100)
  (play t (cycle pitches) 102))

(stop)




(definst c-hat [amp 0.5 t 0.3]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(def metro (metronome 128))

(metro) ; => current beat number
(metro 100) ; => timestamp of 100th beat

(def sidestick (freesound 438))

(def hat2 (freesound 87731))

(dub-kick)

(defn player [beat]
  (at (metro (+ 0.5 beat)) (dub-kick))
  (at (metro (+ 5 beat)) (kick3)
  (at (metro (+ 10 beat)) (kick2))
  (at (metro (+ 15.7 beat)) (sidestick))
  (at (metro (+ 20.3 beat)) (c-hat))

  (apply-by (metro (inc beat)) #'player (inc beat) [])))

(player (metro))

(metro-bpm metro 80)

(stop)








(def chord-prog
  [#{[2 :minor7] [7 :minor7] [10 :major7]}
   #{[0 :minor7] [8 :major7]}])

(def beat-offsets [0 0.1 0.2 1/3  0.7 0.9])

(def metro (metronome 80))

(def root 50)
(def max-range 30)
(def range-variation 30)
(def range-period 8)

(defn beat-loop
  [metro beat chord-idx]
  (let [[tonic chord-name] (choose (seq (nth chord-prog chord-idx)))
        nxt-chord-idx      (mod (inc chord-idx) (count chord-prog))
        note-range         (cosr beat range-variation  max-range range-period)
        notes-to-play      (rand-chord (+ root tonic)
                                       chord-name
                                       (count beat-offsets)
                                       note-range)]
    (dorun
     (map (fn [note offset]
            (at (metro (+ beat offset)) (guitar note 0.3)))
          notes-to-play
          beat-offsets))
    (apply-by (metro (inc beat)) #'beat-loop [metro (inc beat) nxt-chord-idx])))

;;start the music:
(beat-loop metro (metro) 0)

try changing the beat-offsets on the fly
(def beat-offsets [0 0.2 1/3  0.5 0.8])
(def beat-offsets [0 0.2 0.4  0.6 0.8])
(def beat-offsets [0 0.1 0.2  0.3 0.4])
(def beat-offsets [0 0.1 0.11 0.13 0.15 0.17 0.2 0.4 0.5 0.55 0.6 0.8])

(metro-bpm metro 120)

;;to stop call (stop)
(stop)


(def piece [:E5 :A5 :E5 :B5 :E5 :G5 :A5
            :E5 :C6 :E5 :D6 :E5 :B5 :C6
            :E5 :A5 :E5 :B5 :E5 :G5 :A5
            :E5 :C6 :E5 :D6 :E5 :B5 :C6 :E5 :B5])

(def piece2 [:E8 :E7 :E6 :E5 :E4 :E3 :E2 :E1   ])

(defn player
  [t speed notes]
  (let [n      (first notes)
        notes  (next notes)
        t-next (+ t speed)]
    (when n
      (at t
        (sampled-piano (note n)))
      (apply-by t-next #'player [t-next speed notes]))))

(def num-notes 1000)

(do
  (player (now) 200 (take num-notes (cycle piece2))))

(stop)





