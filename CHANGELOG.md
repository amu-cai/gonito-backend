## 3.23.0 (2022-02-19)

Enhancements:

* add full-user-info end-point

## 3.22.0 (2022-01-31)

Enhancements:

* add metadata for whether disclose or not evaluation results for the main test

## 3.21.0 (2022-11-19)

Switch to LTS 19.25

## 3.20.3 (2022-11-18)

Last version before switching to LTS 19.25.

Bug fixes:

* Fix tests

## 3.21.0 (2022-09-08)

Enhancements:

* add WAR and CAR metrics

## 3.20.0 (2022-08-17)

Enhancements:

* add CustomMetric1 (for one of PolEval competitions)

## 3.19.0 (2022-07-21)

Enhancements:

* add NDCG metric

## 3.18.0 (2022-07-01)

Enhancements:

* end-points for listing challenges return the deadline and metric name now

## 3.17.0 (2022-06-25)

Enhancements:

* add tags for challenges
* add end-point for listing challenges matching given tag(s)

## 3.16.4 (2022-04-26)

Improvement:

* Limit the number of parameter graphs (to speed it up)

## 3.16.3 (2022-04-08)

Bug fixes:

* Fix cloning https repos
* Fix issue with MacroAvg
* Teacher can see the achievements now

## 3.16.2 (2022-03-28)

Bug fixes:

* Fix "Wrong SSH Key" during regular submission

## 3.16.1 (2022-03-04)

Bug fixes:

* Fix "Wrong SSH Key" during triggering"

## 3.16.0 (2022-02-11)

Enhancements:

* Add {RMSE,MSE,MAE}-Against-Interval metrics
* Add MacroAvg/* metrics

## 3.15.1 (2022-02-08)

Bug fixes:

* Fix wrong name for Add Probabilistic-Soft2D-F-measure

## 3.15.0 (2022-02-07)

Enhancements:

* Add Probabilistic-Soft2D-F-measure

## 3.14.0 (2022-02-03)

Enhancements:

* Add Improvement metric

## 3.13.0 (2022-01-19)

Enhancements:

* Introduce individual keys

## 3.12.4 (2021-11-13)

Bug fixes:

* Fix random results when querying for submissions that were re-evaluated.

## 3.12.0 (2021-10-29)

Enhancements:

* New p<P> flag

Improvements:

* Results for older tests are shown now

## 3.11.3 (2021-10-09)

Bug fixes:

* Improve item-by-item mode for BIO-F1
* Fix clarifications for submitting solutions
* error

## 3.11.2 (2021-10-01)

Bug fixes:

* Fix links to public submissions

## 3.11.1 (2021-10-01)

Bug fixes:

* Fix links to public submissions


## 3.11.0 (2021-09-25)

Enhancements:

* Add phases to challenges
* When the result of a query is just a single submission with one variant,
  the user is redirected directly to the variant.

Bug fixes:

* Yet another fix the end-point for opening submissions

## 3.10.1 (2021-09-22)

Bug fixes:

* Fix opening submissions

## 3.10.0 (2021-09-1)

Enhancements:

* Leaderboard end-point returns a isOwner flags
* Add make-public end-point for opening challenges

## 3.9.0 (2021-09-02)

Enhancements:

* Add current-time end-point for current server time
* Add format-as-local-time end-point for formatting time stamps
  as a local time

Breaking changes:

* version-info returns version as a list

## 3.8.0 (2021-08-31)

Enhancements:

* Add end-point for returning the token for triggering evaluations
* Line-by-line mode can use expected data from the public challenge
  repository if needed

Fixes:

* Pretty metric names are used in announcements

## 3.7.0 (2021-08-21)

Enhancements:

* Add PerplexityHashed metric
* Add MENULESS option
* Introduce no-internal-git-server scheme
* Announcements can be sent to Discord

Fixes:

* Fix how-to for submitting solutions
* Fix the way scores are formatted for announcements

Changes (might be considered compatibility breaks):

* change `NEW_BEST_RESULT_SLACK_HOOK` to more general `ANNOUNCEMENT_HOOK`
* details for all submissions are shown (including outputs)
* only outputs for the main test set are hidden

## 3.6.1 (2021-08-09)

Improvements:

* The end-point for viewing progress logs returns a self-contained HTML

## 3.6.0 (2021-07-30)

Enhancements:

* Add an end-point for viewing progress logs with web sockets

Bug fixes:

* Improve robustness to broken challenges


## 3.5.0 (2021-07-25)

Improvements:

* Improve handling MultiLabel-F1 when probs are given
* Improve legitibility of submissions with a large number of parameters

## 3.4.0

Enhancements (with GEval):

* Fuzzy matching can be used for Accuracy

## 3.3.0

Improvements:

* Handle DOS/Windows end-of-lines

## 3.2.0

Enhancements:

* Add TEAM_FIELD configuration variable to choose a metadata field from
  which to take the team name of a user
* Add AUTO_TEAM configuration variable to automatically set
  the default team for a submission

## 3.1.0

Enhancements:

* Update to new version of GEval (Haversine and BIO-Weighted-F1 metrics added)
* Add challenge-repo end-point
* Improve generating browsable links


## 3.0.0

Breaking compatibility:

* "version" property is always a list of integers

Improvements:

* Clean up Swagger documentation
* Fix metric names to be shown
* Add the proper end-point for images

## 2.3.0

Changes:

* Switch to new style of Dockerfile
* Challenge slugs can start with a digit

## 2.2.2

Improvements:

* Improve swagger and documentation

## 2.2.1

Bug fixes:

* Logging is fully working now with Nginx
* Add git-annex to the container

## 2.2.0

Bug fixes:

* UTF-8 locale is set in the container

New features:

* A challenge can marked as competition (sorting by submitter/team is enforced then)


## 2.1.0

New features:

* A team captain can invite other members

## 2.0.0

New features:

* users can be organized into teams

Enhancements:

* add end-point for listing tags

Breaking compatibility:

* switch to a different DB schema

## 1.2.0

Enhancements:

* add "version" field for challenges
* add end-point for challenge versions
* no CSRF token is required when submitting a solution via API

## 1.1.0

Improvements

* more fields in leaderboard end-point
* adding an instant pre-check for a submission

## 1.0.0

Start CHANGELOG
