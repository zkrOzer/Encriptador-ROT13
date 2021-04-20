(ns plf06.core
  (:gen-class))

(def caracteres
  (seq "aábcdeéfghiíjklmnñoópqrstuúüvwxyz"))

(def mayusculas
  (seq "AÁBCDEÉFGHIÍJKLMNÑOÓPQRSTUÚÜVWXYZ"))

(def simbolos
  (seq "01234!\"#$%='()*+,-./:;<&>?@[\\]^_`{|}~56789"))

(def s
  (concat simbolos simbolos caracteres mayusculas simbolos))

(defn valoress
  [s]
  (drop 13 s))

(defn tomarvalores
  [s]
  (take (count s) (valoress s)))

(defn ciclo
  [s]
  (flatten (repeat (count s) (tomarvalores s))))

(def f
  (zipmap s (ciclo s)))

(defn rot13
  [s] (cond
        (= (first (rest s)) \space) (apply str (first s) (apply str (map #(get f % %) (rest s))))
        (contains? (into #{} s) \space) (apply str (map #(get f % %) s))
        :else
        (apply str (map #(get f % %) (str (subs s 0 (- (count s) 3)) " " (subs s 7 (count s)))))))

(rot13 (apply str "Cancion #72"))
(rot13 (str "Can" "ción" "#72"))
(rot13 (str "Canción" "#72"))
(rot13 (str "Can" "ción" "#72"))
(rot13 (str "Can" "ción" "#" "72"))
(rot13 (str "c AN" "CIÓN" "#" "72"))

(defn -main
  [& args]
  (if (empty? args)
    (println "No se introdujo nada")
    (if (string? args)
      (println "No se ha ingresado una cadena")
      (print (rot13 (apply str args))))))

