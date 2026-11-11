# FreeMemo Learning Context

FreeMemo organizes personal study materials, practice, assessment, and
progression. This glossary names the concepts that connect those areas.

## Curriculum

**Curriculum**:
A personal, ordered study program owned by one user.
_Avoid_: Course, syllabus, shared curriculum

**Block**:
The main curriculum progression unit. A Block contains ordered Materials and one
Block Exam.
_Avoid_: Module, section, stage

**Material**:
One study source inside a Block, such as a PDF, Markdown document, or Web
Article.
_Avoid_: Resource, content item, document

**Available Block**:
An unlocked Block that the learner can study. Several Blocks can be available
at the same time.
_Avoid_: Current block, open stage

**Focus Block**:
The Available Block that Schedule plans first.
_Avoid_: Current block, active block

**Block Completion**:
The state produced by a passing Block Exam Result.
_Avoid_: Unlock, finished material, passed checkpoint

**Unlock Override**:
Permission to enter the next Block without completing the preceding Block.
_Avoid_: Completion override, forced completion

**Curriculum Completion**:
The state produced when every Block in a Curriculum has Block Completion.
_Avoid_: Reached the end, unlocked all blocks

## Exams

**Material Exam**:
A five-question long-form assessment for one Material. Its score guides study
but does not control progression.
_Avoid_: Material quiz, checkpoint quiz

**Block Exam**:
A five-question long-form assessment that samples priority outcomes from one
Block. A passing result completes the Block.
_Avoid_: Final quiz, block quiz

**Exam Blueprint**:
A coverage map from learning outcomes and source evidence to Essay Questions.
_Avoid_: Question list, generated exam

**Exam Form**:
One frozen version of an Exam Blueprint, its Essay Questions, rubrics, evidence,
and passing threshold.
_Avoid_: Exam template, question set

**Outdated Exam Form**:
An Exam Form whose source Material changed after approval.
_Avoid_: Invalid exam, deleted exam

**Essay Question**:
A long-form prompt that asks the learner to explain, compare, apply, evaluate,
or synthesize source material.
_Avoid_: Card, fact question, short-answer question

**Exam Sitting**:
One learner attempt against one Exam Form.
_Avoid_: Session, run

**Grade Revision**:
One immutable AI evaluation of an Exam Sitting. The latest successful AI Grade
Revision is authoritative.
_Avoid_: Regrade result, mutable grade

**Exam Result**:
The current scored outcome derived from the authoritative Grade Revision of one
Exam Sitting. It includes the score and pass or fail decision.
_Avoid_: Grade Revision, verdict

**Targeted Retry Form**:
A new five-question Exam Form that emphasizes outcomes from a failed Block Exam.
_Avoid_: Mixed-form retry, copied exam

**Timed Quiz**:
The existing timed assessment over short knowledge-graph questions.
_Avoid_: Exam

## Topics

**Source Document**:
The topic at the top of a topic tree. An import makes one for each PDF, EPUB
book, web article, Wikipedia page or video. Every topic in the tree belongs to
it. A topic with no parent is its own Source Document. A Material can name a
Source Document or a topic inside one.
_Avoid_: Material, root, book, document (alone)

**Parent Topic**:
The topic directly above a topic in its tree. For a topic one level down, the
Parent Topic is also the Source Document.
_Avoid_: Source Document, container

## Learn

**Learn Session**:
The run of due topics that a user reads one after the other, started from the
Learn page. The queue is fixed when the session starts.
_Avoid_: Review session, study session, Learn page

**Next**:
The action that ends the current topic in a Learn Session, schedules its next
review, and shows the next topic in the queue.
_Avoid_: Advance, skip

**Topic View**:
The part of the Learn Session screen that shows one topic: its body, its
toolbars, its cards, and the panes around it.
_Avoid_: Viewer, reader, topic page

## Service Commitments

**Availability**:
The share of minutes in a window in which a customer can reach FreeMemo from
the public internet. Failures between the customer and the server count.
_Avoid_: Uptime (alone), internal uptime

**Uptime Target**:
The published Availability that FreeMemo aims to meet. Missing it gives no
compensation.
_Avoid_: SLA, uptime guarantee, uptime promise

**SLA**:
A commitment that compensates the customer when Availability falls below a
stated level. FreeMemo does not offer one.
_Avoid_: Uptime Target

**Backup**:
A copy of every account's data that the operator keeps to restore the service
after a loss.
_Avoid_: Export, Account Archive, snapshot

**Account Archive**:
A copy of one account's data that the customer makes and keeps.
_Avoid_: Backup, export file

**Paying Customer**:
An account with at least one completed purchase, of credits or of a Sync
License, in the last 12 months. Granted credits do not count.
_Avoid_: Paid user, subscriber, customer (alone)

## Distribution

**Local Install**:
FreeMemo that runs on the customer's own study computer, with its own local
database, and syncs with a FreeMemo account on freememo.net. It is the paid way
to use FreeMemo offline.
_Avoid_: Self-host, self-hosted, desktop app (for this product)

**Independent Self-Host**:
A FreeMemo server that a person runs from the public code, with no freememo.net
account. FreeMemo gives it no support.
_Avoid_: Self-host (alone), official self-host

**Release**:
One publication of FreeMemo, made by each push of `unstable`: freememo.net
runs it, and the public GitHub repository offers it as its latest release.
_Avoid_: Deploy, build, version (alone)

**Release Version**:
The name of a Release: the UTC time of its commit, written as the two-digit
year, the month, and the minute of that month (`26.11.13832` is 10 November
2026, 14:32). A later Release always has a larger Release Version.
_Avoid_: Major, build number, semver

**App Update**:
The app part of a Release, which a Local Install downloads by itself and starts
using at its next start. It needs no new installer.
_Avoid_: Patch, hot update, upgrade

**Runtime**:
The parts of a Local Install that an App Update cannot change: the Java
runtime, PostgreSQL and the bundled media tools. A new Runtime needs a new
installer.
_Avoid_: Platform, bundle, major

**Sync Protocol**:
The number that names how a Local Install talks to freememo.net. A Local
Install syncs only while its Sync Protocol matches the one freememo.net runs.
_Avoid_: Major, desktop version, API version

**Sync License**:
A one-time purchase on a FreeMemo account that lets its Local Installs sync
with freememo.net and use AI through freememo.net, on every Release, with no
end date.
_Avoid_: Desktop license, subscription, versions 1 and 2
