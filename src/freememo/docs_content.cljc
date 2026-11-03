(ns freememo.docs-content
  "Public documentation copy. Rendering and request handling live elsewhere.")

(def cycle-steps
  [{:title "Choose material" :detail "Start with a source you want to understand."}
   {:title "Review the exam" :detail "Check its coverage, question formats, and points."}
   {:title "Answer from memory" :detail "Type your answers or write numbered answers on paper."}
   {:title "Check the transcript" :detail "Compare handwriting with the transcript before grading."}
   {:title "Inspect the gaps" :detail "Read the answer evidence and source evidence behind each gap."}
   {:title "Study and reassess" :detail "Choose follow-up study, then check the same learning target again."}])

(def article
  [{:id "purpose" :title "Use an exam to choose your next study task"
    :paragraphs ["The comprehension loop connects reading, assessment, feedback, and further study. Its purpose is to find gaps and guide study."
                 "A cycle starts when you choose. It does not require a weekly schedule or a study deadline."
                 "Diagnostic results do not complete a curriculum block. They do not automatically change your quiz review schedule or mark facts as mastered."]}
   {:id "material" :title "1. Start with material you can check"
    :paragraphs ["Import the material and check its text first. Poor source text can produce poor questions and misleading feedback."
                 "Extracted facts help identify what a question tests. Keep the source passages available to check those facts and their connections."
                 "Open Comprehension. Choose material, set a target duration, and select Start cycle. Duration is an estimate, not a countdown."
                 "You can take longer than the estimate. Extra time does not reduce your grade."]
    :links [["/docs/how-to/importing-content" "Import material"]
            ["/docs/how-to/knowledge-graph" "Check extracted facts"]
            ["/comprehension" "Open Comprehension"]]}
   {:id "exam" :title "2. Review coverage before you answer"
    :paragraphs ["The exam can combine fill-in-the-blank, multiple-choice, short essay, and longer essay questions. It omits formats the material cannot support."
                 "A target of 20 minutes or more gets at least one essay, unless the material holds only isolated terms and values. The Coverage panel names each format the exam left out, and why. It also lists each generated question that failed the checks."
                 "Review the assessed topics, reasoning demands, omitted formats, question counts, and available points. A short exam samples your understanding."
                 "A multiple-choice question can test reasoning. An essay can test recall. Judge the task by what it asks you to do."
                 "If the source cannot support an assessment, add or correct material first."]}
   {:id "paper" :title "3. Answer by hand or type"
    :paragraphs ["Write on ordinary paper. Number each answer with its question number. You can use several pages for one answer."
                 "A multiple-choice question has four options, A to D. On screen, select one. On paper, write its letter."
                 "Use Print answer sheet if you want numbered questions and answer space. The sheet is optional."
                 "Keep your first answers separate from later improvements. The first attempt shows what you could do before further study."
                 "Select Upload handwritten answers to add a PDF, PNG, or JPEG. Typed answers use the same assessment and feedback path."]}
   {:id "transcript" :title "4. Check what the system read"
    :paragraphs ["Transcription starts when the upload ends, and it uses credits. When the transcript is ready, compare the original pages with the extracted text and the assigned question numbers."
                 "Correct transcription errors before you submit. Check symbols, negation, crossed-out text, equations, and answers that continue on another page."
                 "A transcription correction restores what you wrote. An improved answer adds knowledge after the attempt. These changes have different meanings."
                 "If the scan remains unclear, the result needs review. Unclear handwriting is not evidence that you misunderstood the material."]}
   {:id "gaps" :title "5. Inspect the diagnosis"
    :paragraphs ["Select Submit answers after you check the answers and question assignments. Read the feedback alongside your answer and the cited source."
                 "A useful gap names a learning target and explains the evidence. A total score alone cannot tell you what to study."]
    :items ["Recall: a required fact was absent or incorrect."
            "Connection: the answer did not connect the relevant facts."
            "Application or reasoning: the answer did not use the facts to explain or solve the task."
            "Uncertain result: transcription, source evidence, or grading needs review before you accept the diagnosis."]}
   {:id "study" :title "6. Choose study, then check again"
    :paragraphs ["Select Study source to return to the evidence for a gap. Select Practice questions to work on that target."
                 "Choose Reassess when you want another check. Compare the new evidence with the original answer and gap."
                 "Studying does not close a gap by itself. A later answer can provide evidence of improvement. One successful answer does not prove permanent mastery."
                 "A question that combines several facts provides evidence about the whole task. It does not prove equal knowledge of every linked fact."]}
   {:id "science-example" :title "Worked example: photosynthesis"
    :paragraphs ["Suppose your source explains light-dependent reactions and carbon fixation. A recall question asks where the light-dependent reactions occur."
                 "A connection question asks how their products support carbon fixation. An application question asks what happens when light becomes unavailable."
                 "You name the chloroplast correctly but confuse the roles of ATP and carbon dioxide. The useful gap concerns that connection."
                 "Review the cited passage, explain the connection in your own words, and answer a new application question. Keep the earlier answer for comparison."]}
   {:id "humanities-example" :title "Worked example: a historical argument"
    :paragraphs ["Suppose your source discusses economic pressures and political events before a revolution. Recall questions check the events and their sequence."
                 "An essay asks you to compare two explanations using evidence. Naming both explanations does not show that you can weigh them."
                 "Your answer lists events but gives no evidence for its causal claim. The useful gap concerns the argument and its support."
                 "Return to the cited passages, compare the explanations, and write a new argument. Judge the new answer against the same target."]}
   {:id "research" :title "What research supports"
    :paragraphs ["Retrieval practice can improve later learning. Transfer depends on the task, initial success, and how learners use their knowledge."
                 "Practice that requires higher-order thinking can support higher-order answers. Fact recall alone does not guarantee that transfer."
                 "Assessment design starts with a learning target, the evidence an answer should provide, and a task that elicits that evidence."
                 "Target duration helps you plan study time. Research does not establish it as a better interface control than question counts."
                 "The complete FreeMemo loop is a product design. These studies do not validate every generated question, AI grade, or diagnosis."]
    :links [["https://rickardlab.ucsd.edu/pdf/PR_2018.pdf" "Pan and Rickard: retrieval practice and transfer"]
            ["https://pdf.poojaagarwal.com/Agarwal_2019_JEdPsych.pdf" "Agarwal: retrieval practice and higher-order learning"]
            ["https://www.sri.com/publication/education-learning-pubs/evidence-centered-assessment-design-layers-structures-and-terminology-padi-technical-report-9/" "SRI: evidence-centered assessment design"]
            ["https://pubmed.ncbi.nlm.nih.gov/31017518/" "Raymond and Grande: assessment blueprints"]]}
   {:id "limits" :title "Check uncertain evidence"
    :paragraphs ["Handwriting recognition can change a symbol or complete an unfinished expression. A plausible transcript can still misrepresent your answer."
                 "AI feedback can also miss sound reasoning or accept weak reasoning. Inspect the rubric, answer evidence, and source evidence before acting."
                 "Use uncertain results as a reason to review evidence. Do not treat them as confirmed learning gaps."
                 "Diagnostic cycle history and answer scans stay on this FreeMemo instance. Account export and desktop sync do not include them."]
    :links [["https://aclanthology.org/2026.findings-acl.751/" "EDU-CIRCUIT-HW: handwritten solutions and grading"]
            ["https://arxiv.org/html/2603.00895v1" "Handwritten calculus recognition study (preprint)"]]}])

(def faq
  [{:question "Do I need an account to read these docs?"
    :answer "No. Documentation is public. Your material, answers, and study history remain inside your account."}
   {:question "Does a diagnostic exam complete a curriculum block?"
    :answer "No. The comprehension loop guides study. Curriculum exams keep their own completion rules."}
   {:question "Must every exam use all four formats?"
    :answer "No. The exam can omit a format when the material does not support it. Review the coverage before answering."}
   {:question "Do I need a printed answer sheet?"
    :answer "No. Ordinary paper with numbered answers works. Print answer sheet provides an optional template."}
   {:question "Can I correct a handwriting recognition error?"
    :answer "Yes. Compare the scan with its transcript and question assignments before you submit answers for grading."}
   {:question "Does a high score prove mastery?"
    :answer "No. A result describes evidence from one assessment. Later practice and reassessment can provide more evidence."}
   {:question "Can I move diagnostic history and scans to another instance?"
    :answer "Not through account export or desktop sync. Diagnostic cycle history and answer scans currently stay on the instance where you created them."}
   {:question "Does extra time reduce my grade?"
    :answer "No. Target duration is an estimate. You can take longer without a time penalty."}
   {:question "Why does Help still exist?"
    :answer "Existing Help links, contextual help, and command-palette help still work. How-to guides use the same instructions."}])
