(ns org.nfrac.hatto.data)

(defrecord Entity [entity-type objects joints])

(defrecord BodyPois [body pois])

(defrecord PointState [point position velocity angular-velocity])

