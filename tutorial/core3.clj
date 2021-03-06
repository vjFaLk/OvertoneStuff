(ns tutorial.core
  (:use [overtone.core]
        [tutorial.synthesizers]
        [tutorial.patterns]
        [tutorial.rhythmic]
        [tutorial.fx]
        [tutorial.samples]))

(use 'overtone.synth.sampled-piano)
(boot-external-server)


;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;First Load an kit before load the fx
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;Load 1º
(def master (audio-bus))

;Fx
(def rev (reverb-tooloud :in-bus master))
(def cmp (compressor-tooloud :in-bus master))
(def lim (limiter-tooloud :in-bus master))

;kit 1
(def k1 (kicki :out-bus master))
(def s1 (snr :out-bus master :bpm 140))

;kit 2
(def k2 (kickii :out-bus master))
(def s2 (snr :out-bus master :bpm 280))


;kit 3 (11!!!)
(def k3 (kickiii :out-bus master))
(def s3 (snr :out-bus master :bpm 560))

;kills
(kill k1)
(kill k2)
(kill k3)

(kill s1)
(kill s2)
(kill s3)

;woobles

(def dub1 (dub-base-i :out-bus master))
; Fill
(def fill (p (cycle (pattern derezzed 140))))
(def dub2 (dub-base-ii :out-bus master))


(ctl dub1 :wobble 6)
(ctl dub1 :note 80)
(ctl dub1 :note 40)


(ctl dub2 :wobble 3)
(ctl dub2 :note 70)
(ctl dub2 :wobble 6)
(ctl dub2 :note 40)

;stop
(stop)

;kill dubs

(kill dub1)
(kill dub2)


; background
(def base
  (demo 140
      (let [bpm 140
       notes [36 30 35 40 45 30 80 79 30 40 45 30 20 90]
       trig (impulse:kr (/ bpm 140))
       freq (midicps (lag (demand trig 0 (dxrand notes INF)) 0.25))
       swr (demand trig 0 (dxrand [1 6 6 2 1 2 4 8 6 3 16 1 6 6 2 12 2 4 8 6 3] INF))
       sweep (lin-exp (lf-tri swr) -1 1 [80 30 85 40 78 25 80 79 80 40] 3000)
       wob (apply + (wobble-saw (* freq [0.99 1.01])))
       resonant-pad (* freq [0.99 1.01])
       wob (lpf wob sweep)
       wob (* 1 (normalizer wob))
       wob (+ wob (bpf wob 1500 2))
       wob (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))]

       (out master (+ wob) 1))))

; Backgraund Mouse Wooble
(def base-mouse
  (demo 140
      (let [bpm 140
       notes [(mouse-x:kr 30 40) (mouse-y:kr 40 70) 30 40 70 80]
       trig (impulse:kr (/ bpm 140))
       freq (midicps (lag (demand trig 0 notes) 0.25))
       swr (demand trig 0 (dxrand [(mouse-x:kr 1 6) (mouse-y:kr 6 12)] INF))
       sweep (lin-exp (lf-tri swr) -1 1 notes 3000)
       wob (apply + (saw (* freq [(mouse-x:kr 0.8 0.9) (mouse-y:kr 1 1.1)])))
       wob (lpf wob sweep)
       wob (* 1 (normalizer wob))
       wob (+ wob (bpf wob 1500 2))
       wob (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))

       kickenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0 (dseq [[1 0 0 1 0 0 1 0 1 0 0 1 0 0 0 0] [1 0 0 1 0 0 1 0 1 0 0 1 0 0 10 0] [1 0 0 0 0 0 1 0 1 1 0 1 0 0 0 0] [1 0 0 1 0 0 1 0 1 0 0 1 0 0 1 1]] INF))) 0.7)
       kick (* (* kickenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
       kick (clip2 kick 3)

       snare (* 3 (pink-noise [1 1]) (apply + (* (decay (impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
       snare (+ snare (bpf (* 4 snare) 2000))
       snare (clip2 snare 3)]

   (out master (pan2 (+ wob kick snare) 2)))))

;kill background
(kill base)
(kill base-mouse)


(def piecei   [36 30 35 40 45 30 80 79 30 40 45 30 20 90])
(def pieceii  [:C5 :D#5 :G5 :C5 :C4 :D#4 :G4])

(defn player
  [t speed notes]
  (let [n     (first notes)
        notes (next notes)
        t-next (+ t speed)]
    (when n
      (at t
          (resonant-pad (note n)))
          (sampled-piano (note n)))
      (apply-at t-next #'player [t-next speed notes])))

(def num-notes 25)
(do
  (player (now) 128 (take num-notes (cycle piecei)))
  (player (now) 64 (take num-notes (cycle pieceii))))

;stop all
(stop)


;Samples

(def bar-dur (atom 1000))

(play-rhythm patterns* bar-dur)

(defn update-pat!
  [key pat]
  (swap! patterns* (fn [patterns key new-pat]
                     (let [[samp pat] (get patterns key)]
                       (assoc patterns key [samp new-pat])))
         key pat))

(update-pat! :clap  [[_ X]])
(update-pat! :cy    [[_ X] [_ X]])
;(update-pat! :bass  [[_] [X] [_ [_ _] _]])
(update-pat! :snare [[_]])
(update-pat! :hhos  [[X] [_ [X X X _]]])

;stop drums
(update-pat! :clap  [[_]])
(update-pat! :cy    [[_]])
(update-pat! :bass  [[_]])
(update-pat! :snare [[_]])
(update-pat! :hhos  [[_]])


;stop all
(stop)
