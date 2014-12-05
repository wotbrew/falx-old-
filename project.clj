(defproject falx "0.1.0-SNAPSHOT"
            :description "Tactical Dungeoneering"
            :url "http://github.com/danstone/falx"
            :license {:name "Eclipse Public License"
                      :url "http://www.eclipse.org/legal/epl-v10.html"}
            :dependencies [[org.clojure/clojure "1.7.0-alpha1"]
                           [gdx-loopy "0.1.0-SNAPSHOT"]
                           [gdx-2d "0.1.0-SNAPSHOT"]
                           [silc "0.1.3"]
                           [cheshire "5.3.1"]
                           [clj-tuple "0.1.7"]
                           [org.clojure/core.memoize "0.5.6"]]
            :global-vars {*warn-on-reflection* true}
            :java-source-paths ["src-java"]
            :repositories [["sonatype"
                            "https://oss.sonatype.org/content/repositories/releases/"]]
            :profiles {:dev {:dependencies   [[org.clojure/tools.trace "0.7.8"]
                                              [org.clojure/test.check "0.6.1"]]
                             :resource-paths ["test-resources"]}})
