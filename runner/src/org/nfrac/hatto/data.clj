(ns org.nfrac.hatto.data)

(defrecord Entity [entity-type components joints])

(defrecord BodyPois [body pois])

(defrecord PointState [point position velocity])

