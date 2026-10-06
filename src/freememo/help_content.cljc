(ns freememo.help-content
  "Help's content and the lookups over it, as plain data and plain fns.

   The Help page renders it and the command palette searches it, so it
   requires no Electric namespace — the palette must not require a page
   (plans/searchable-help.md 1.1). Text is resolved through `resolved-text`,
   the one place chord tokens become the chord a user sees, so display and
   search can never disagree about what a step says."
  (:require
   [clojure.string :as str]
   [freememo.commands :as commands]))

(def sections
  "Every Help section, in reading order.

   `:slug` is hand-written and never derived from `:title`, so renaming a
   title breaks no /help/<slug> link. A step's `:commands` holds the registry
   ids that step documents; `undocumented` holds the rest, and
   help_content_test asserts the two cover the registry between them."
  [{:slug "getting-around"
    :title "Getting around"
    :intro "The sidebar at the left holds every place in FreeMemo, in the order you use them: import, read, review, plan, explore."
    :steps [{:label "Open the sidebar"
             :detail "Rest the pointer on the sidebar, or press Tab to move into it. It opens over the page and closes when you leave it. Open Sidebar in the command palette puts focus in it, and Esc closes it."
             :commands #{:open-nav-rail}}
            {:label "Keep it open ({toggle-nav-rail})"
             :detail "Keep open, at the foot of the sidebar, docks it beside the page. In a window 900px wide or less it opens over the page instead. On a phone, ☰ opens it."
             :commands #{:toggle-nav-rail}}
            {:label "Continue"
             :detail "Continue opens the last document you opened in the Viewer. A learn session and a subset review do not change it: Continue returns to the document you chose to read, and a learn session picks its items from a queue."
             :commands #{:go-continue}}
            {:label "Learn's count"
             :detail "The number on Learn is how many topics are due, the same count Home shows."
             :commands #{}}]}
   {:slug "importing-content"
    :title "Importing content"
    :intro "Everything starts on the Import tab. Drop a file on it, paste on it, or pick a source card. The sections after this one cover each source."
    :steps [{:label "Drop or browse"
             :detail "Drop a file anywhere on the Import page, or click Browse files. The box reads Drop a file, paste a link, or paste text. The chips under it name each accepted type and its size limit."
             :commands #{}}
            {:label "Paste on the page"
             :detail "Paste anywhere on the Import page, outside a text field. A web address on its own starts a link import at once. HTML or a copied web page opens Paste as HTML. Other text opens Paste as Markdown. Give it a title, then Import."
             :commands #{}}
            {:label "Drop several files"
             :detail "Several videos import together and can become one playlist. Files of different kinds are refused. Of several other files, FreeMemo imports only the first."
             :commands #{}}
            {:label "Find the other sources"
             :detail "Link, Audio and Video are the main cards. More sources opens New Topic, Paste, Score, Zotero, Code, Poem and Live Document."
             :commands #{}}
            {:label "Which source should I use?"
             :detail "A PDF, EPUB, HTML or Markdown file: drop it. A SuperMemo collection: zip its folder and drop it. A built Sphinx site: zip it and drop it. A FreeMemo account export: drop the .zip. A web page or Wikipedia article: Link. A recording: Audio. A video: Video. Your own text: New Topic or Paste. Sheet music and a recording: Score. A paper in Zotero: Zotero. A code repository: Code. A poem package: Poem. Pages on paper: Live Document."
             :commands #{}}]}
   {:slug "import-file"
    :title "Import a PDF, EPUB, HTML or Markdown file"
    :intro "Bring in a document from your computer. It opens in the Viewer."
    :steps [{:label "Pick the file"
             :detail "Drop it on the Import page, or click Browse files. PDF, EPUB, HTML (.html, .htm) and Markdown (.md, .markdown) are accepted. The default limits are 100 MB for PDF, HTML and Markdown, and 4 GB for EPUB."
             :commands #{:import-upload :import-file :import-document}}
            {:label "Confirm the type"
             :detail "A PDF or an EPUB asks you to confirm: Import as PDF or Import as EPUB. An HTML or Markdown file asks for a title, then Import."
             :commands #{}}
            {:label "Shrink EPUB images (optional)"
             :detail "Before you import an EPUB, ▸ Advanced → Images offers Reduce size, the default, or Strip images, which keeps the text only."
             :commands #{}}
            {:label "What you get"
             :detail "A PDF becomes one topic with a child topic for each page. An EPUB becomes one topic with a child topic for each chapter. An HTML or Markdown file becomes one topic."
             :commands #{}}]}
   {:slug "import-supermemo"
    :title "Import a SuperMemo collection"
    :intro "Move a SuperMemo 17, 18 or 19 collection into FreeMemo, with its tree, its cards and the schedule of its topics."
    :steps [{:label "Zip the collection"
             :detail "Make a .zip or .7z of the collection folder. It must hold info/contents.dat and info/ElementInfo.dat."
             :commands #{}}
            {:label "Drop it"
             :detail "Drop the archive on the Import page, then Import as SuperMemo Collection. An archive can be up to 16 GB."
             :commands #{:import-supermemo}}
            {:label "What carries over"
             :detail "Topics and tasks become topics with the same titles. Folders (concept groups) are removed, and their children move up. Items become cards on the nearest topic. References become sources. Images, colours, fonts and extract highlights are kept. Each topic keeps its interval, A-factor, review dates and repetition history. The order of the priority queue becomes FreeMemo priority."
             :commands #{}}
            {:label "What stays behind"
             :detail "Grades, lapses, the repetition history of items, U-factors, element layout, concepts, tasklists, subsets and the SuperMemo algorithm state."
             :commands #{}}
            {:label "After the import"
             :detail "FreeMemo opens the root topic. It shows no summary of what it imported."
             :commands #{}}]}
   {:slug "import-sphinx"
    :title "Import a Sphinx documentation site"
    :intro "Read a project's built Sphinx documentation as a topic tree."
    :steps [{:label "Zip the built site"
             :detail "Make a .zip of the built HTML site. It must hold .buildinfo, objects.inv and searchindex.js."
             :commands #{}}
            {:label "Drop it"
             :detail "Drop the .zip on the Import page, then Import as Documentation Site."
             :commands #{}}
            {:label "What you get"
             :detail "Each page becomes a topic, nested the way the site's folders are. Links between pages open the imported topics. The index, search and 404 pages are skipped."
             :commands #{}}]}
   {:slug "import-account"
    :title "Move your account to another FreeMemo"
    :intro "Copy your whole account from one FreeMemo server to another."
    :steps [{:label "Export"
             :detail "Settings → Account → Download export (.zip). The file holds your documents and their files, your cards and their schedule, the knowledge graph, and your settings. A large account takes a while to start downloading."
             :commands #{}}
            {:label "Import"
             :detail "On the other server, drop the .zip on the Import page, then Import as FreeMemo Account Export."
             :commands #{:import-account}}
            {:label "What happens"
             :detail "The import adds to the account there. It replaces nothing, and a setting that already exists keeps its value. FreeMemo then opens the Library."
             :commands #{}}
            {:label "Importing again"
             :detail "An export that was already imported is refused. An interrupted import continues when you drop the same file again."
             :commands #{}}]}
   {:slug "import-link"
    :title "Import from a link"
    :intro "Bring in a web page, a Wikipedia article, or a PDF or EPUB on the web. A link import uses no credits."
    :steps [{:label "Fetch"
             :detail "Import → Link, paste the address, and Fetch. An address pasted on the Import page starts the import at once."
             :commands #{:import-link}}
            {:label "What is accepted"
             :detail "Wikipedia articles, mobile links included, and any public web page. An arXiv PDF link imports the arXiv abstract page. A link to a PDF or an EPUB asks you to confirm, as a file does."
             :commands #{}}
            {:label "What is refused"
             :detail "Links to images, audio, video, JSON, XML, plain text and ZIP files. FreeMemo does not follow redirects, so paste the final address. Some sites block fetching: copy the page in your browser and use Paste."
             :commands #{}}
            {:label "A link you imported before"
             :detail "FreeMemo opens the topic you already have. It makes no second copy."
             :commands #{}}]}
   {:slug "import-audio"
    :title "Import audio"
    :intro "Bring in a recording, then transcribe it into text you can study."
    :steps [{:label "Upload"
             :detail "Import → Audio, drop or pick the file, then Import as Audio. MP3, M4A, WAV, FLAC and OGG are accepted, up to 25 MB. An MP4 or WebM file goes to Video."
             :commands #{:import-audio}}
            {:label "Transcribe"
             :detail "The topic starts empty. Transcribe on the toolbar writes the transcript into the editor. If the editor already holds text, FreeMemo asks before it replaces it. Transcription uses credits and stops after 120 seconds."
             :commands #{:transcribe}}
            {:label "Set the language"
             :detail "Settings → AI Features → Transcription language. Auto-detect is the default. Pick the language when Auto-detect gets short or quiet audio wrong."
             :commands #{}}]}
   {:slug "import-video"
    :title "Import video"
    :intro "Bring in one video or a playlist, with a transcript you can read and extract from."
    :steps [{:label "Add the files"
             :detail "Import → Video, then drop files or click the box. MP4, MKV, WebM, OGV and MOV are accepted, up to 8 GB each. ＋ Add more adds files. FreeMemo takes files only. It cannot import a YouTube or other video link."
             :commands #{:import-video}}
            {:label "Choose Upload as"
             :detail "Separate documents makes one topic for each video. New playlist makes a playlist topic with the videos under it. Add to existing playlist adds them to a playlist you have."
             :commands #{}}
            {:label "Transcribe after upload"
             :detail "On by default, with an estimate of the credits it uses. Turn it off to transcribe later."
             :commands #{}}
            {:label "Upload"
             :detail "Upload starts it. You can close the dialog, and the upload continues in Activity. A file that is not MP4 is converted after upload. Stored video uses credits over time, and playback stops when storage credit runs out."
             :commands #{}}
            {:label "Use the transcript"
             :detail "The transcript pane lists the segments. Click one to jump to it. Transcribe or Re-transcribe makes the transcript. Copy puts it in the editor."
             :commands #{:process-video}}
            {:label "Extract a range"
             :detail "Drag on the waveform to mark a range. ▶ range plays it and ⟲ loop repeats it. Extract range makes a child topic for that range, with its part of the transcript."
             :commands #{:extract-video}}]}
   {:slug "new-topic"
    :title "Write a new topic"
    :intro "Start a topic with no imported content, and write in it."
    :steps [{:label "Create"
             :detail "Import → More sources → New Topic, type a title, and Create. A blank title becomes New Topic. The empty topic opens in the Viewer."
             :commands #{:new-topic}}]}
   {:slug "import-paste"
    :title "Paste HTML or Markdown"
    :intro "Bring in text you copied, as Markdown or as a web page."
    :steps [{:label "Open Paste"
             :detail "Import → More sources → Paste, or paste on the Import page."
             :commands #{:import-paste}}
            {:label "Fill it in"
             :detail "Pick the Format: Markdown, the default, or HTML. Give a Title. For HTML, you can add a Source URL (optional). Paste the content, then Import."
             :commands #{}}
            {:label "What you get"
             :detail "HTML becomes a web topic with its scripts removed. An empty title is taken from the first heading. Markdown becomes a Markdown topic."
             :commands #{}}]}
   {:slug "score-cards"
    :title "Sheet-music (Score) cards"
    :intro "Turn a score PDF and a recording into audio ↔ notation cards."
    :steps [{:label "Create it"
             :detail "Import → More sources → Score. Pick the Sheet music (PDF) and the Recording, then Import Score. The recording can be up to 200 MB."
             :commands #{:import-score}}
            {:label "Select the audio"
             :detail "Drag directly on the waveform strip to mark the passage you want to hear."
             :commands #{}}
            {:label "Mark the notation"
             :detail "Click Notation, drag a rectangle over the matching bars on the page, then Done."
             :commands #{}}
            {:label "Add the card"
             :detail "Add card ▾ → Audio front, Sheet front, or Both. Both makes two Anki notes that share the same media."
             :commands #{:add-score-group}}
            {:label "Edit a card"
             :detail "Click a score card's row to edit it. Update cards saves the change, and Duplicate makes a copy."
             :commands #{:update-score-group}}]}
   {:slug "import-zotero"
    :title "Import from Zotero"
    :intro "Bring in a PDF from your own Zotero library, with its citation."
    :steps [{:label "Set it up once"
             :detail "Settings → Zotero → Download FreeMemo for Zotero (.xpi). In Zotero, Tools → Plugins → gear icon → Install Plugin From File. In Zotero → Settings → Advanced, allow other applications on this computer to communicate with Zotero. Then turn on Enable Zotero import and press Test Connection."
             :commands #{}}
            {:label "Pick an item"
             :detail "Import → More sources → Zotero. Zotero must be running on this computer. Filter the list, then click an item. If it has several PDFs, pick one."
             :commands #{:import-zotero}}
            {:label "What you get"
             :detail "The PDF opens in the Viewer, and its Zotero citation becomes the document's bibliography. Only PDFs import. Zotero 7 to 10 are supported."
             :commands #{}}]}
   {:slug "learning-a-codebase"
    :title "Import code"
    :intro "Study a code repository as a topic tree. Import it from GitHub, GitLab or Forgejo, or upload a .zip."
    :steps [{:label "From a forge"
             :detail "Import → More sources → Code. Pick the Forge and the Host. A self-hosted server works. Enter the Repository as owner/name, or paste its address. Leave Branch, tag or commit empty for the default branch. Then Download."
             :commands #{:import-code}}
            {:label "Private repositories"
             :detail "Add a token in Settings → Git Forges, then Save token. FreeMemo only reads. A public repository needs no token."
             :commands #{}}
            {:label "Choose what to import"
             :detail "Pick the Directories and the File extensions, then Import. A file whose extension you leave out is still a topic, with no source. One import can make up to 20,000 topics. You can close the dialog, and a notice appears when the import is done."
             :commands #{}}
            {:label "What you get"
             :detail "A topic for the repository, one for the commit, one for each directory and one for each file. Only file topics come up for review. The source shows in a read-only code pane: select lines and Copy to notes. A forge import extracts no code facts."
             :commands #{}}
            {:label "Read without the notes editor"
             :detail "To give the code pane the full width, run Toggle Editor pane from the command palette. Run it again to show the editor. The choice applies to the whole repository and stays after a reload. Generate, Extract and Add new card still take the text you select in the code pane. The command also works on a PDF and on a video."
             :commands #{:toggle-editor-pane}}
            {:label "The repository's home"
             :detail "The repository topic opens on its home, not on a notes editor. The home shows where the repository comes from, its walkthroughs, and its imported commits. Repository, commit and directory topics have no notes, no toolbar and no card table. A directory topic shows nothing: open its files in the hierarchy."
             :commands #{}}
            {:label "Update"
             :detail "On the repository's home, Update adds the newest commit as a new commit topic, with the same selection. Imported commits lists it, newest first, with its message and its number of files."
             :commands #{}}
            {:label "Add files"
             :detail "On the repository's home, Add files… opens the directory and extension picker on the newest imported commit. What was selected stays selected: you can only add. Click Add, and the new files go into that commit. A directory you add above an imported one becomes its parent, and the imported one keeps its notes. A file or directory you deleted is not added back. Later updates use the larger selection."
             :commands #{:add-repository-files}}
            {:label "Read a commit"
             :detail "A commit topic shows the commit's message, author, date, full id with Copy, branch or tag, and number of changed files. FreeMemo reads them from its copy of the repository each time. Walk through this commit starts a walkthrough of that commit alone."
             :commands #{:walk-through-commit}}
            {:label "From a .zip"
             :detail "In the Code dialog, click Upload a .zip file instead, or drop the .zip on the Import page. Only Clojure files (.clj, .cljc, .cljs) are kept, one topic for each file. `git archive --format=zip -o repo.zip HEAD` makes a clean .zip."
             :commands #{}}
            {:label "Code facts (.zip only)"
             :detail "Extract facts into the knowledge graph is off by default. Turn it on to analyse the code in the background. Definitions, calls, requires, arglists and docstrings then appear in Facts for the repository. You can also do this later from the Knowledge tab."
             :commands #{}}]}
   {:slug "import-poem"
    :title "Import a poem"
    :intro "Read an Arabic poem verse by verse, with notes from its commentaries, and make cards from it."
    :steps [{:label "Get a package"
             :detail "A poem package is a .zip with manifest.json, verses.json, notes.json and sources.json at its root. FreeMemo checks the package and names the first rule it breaks."
             :commands #{}}
            {:label "Import it"
             :detail "Import → More sources → Poem, pick the .zip, then Import as Poem. The summary shows the title, the poet, the meter and the number of verses and notes."
             :commands #{:import-poem}}
            {:label "Read it"
             :detail "The poem is one read-only topic. Tap a word to open الشرح (Commentary) in the right panel, with the word's gloss and الإعراب (grammatical analysis). Each note names the book and page it came from."
             :commands #{:show-poem-panel}}
            {:label "Make cards"
             :detail "In the word view, بطاقة البيت (verse card) hides the second half of the verse. بطاقة المعنى (meaning card) and بطاقة الإعراب (analysis card) ask about the word. حفظ البيت (save verse) makes an extract. اشرح هذا البيت (explain this verse) shows the meaning and commentary of the verse."
             :commands #{}}]}
   {:slug "live-document"
    :title "Live Document"
    :intro "Photograph pages into a PDF you keep adding to."
    :steps [{:label "Start one"
             :detail "Import → More sources → Live Document. FreeMemo creates a document named Live Document and the date, and opens it."
             :commands #{:create-live-doc}}
            {:label "Add photos"
             :detail "＋ Add photos, then Upload images or Take photo. HEIC photos are converted."
             :commands #{:append-images}}
            {:label "Edit, then add"
             :detail "↻ Rotate 90° turns a photo, and dragging crops it. Done adds the photos as pages. Nothing uploads before Done. Later, ＋ on the toolbar adds more."
             :commands #{}}]}
   {:slug "studying-a-pdf"
    :title "Studying a PDF"
    :intro "Turn a PDF into studied, card-ready material, one page at a time."
    :steps [{:label "Import it"
             :detail "Drop the PDF on the Import tab, or click Browse files. Link imports a PDF from the web."
             :commands #{}}
            {:label "Read & navigate"
             :detail "Open it from Library, or from Continue in the sidebar. The page arrows or the page box move you; the zoom menu fits width or whole page."
             :commands #{:prev-page :next-page}}
            {:label "Extract the text ({scan})"
             :detail "Scan Page runs OCR on the current page into clean, editable HTML. Copy-text pulls the PDF's own text layer when it has one."
             :commands #{:scan :copy-text}}
            {:label "Edit & split ({extract})"
             :detail "Fix the text in the editor. Select a passage and Extract turns it into a child topic linked back to the page."
             :commands #{:extract}}
            {:label "Mark done ({done})"
             :detail "Mark a page Done to drop it from your review queue while keeping the content."
             :commands #{:done}}]}
   {:slug "boox-neoreader-highlights"
    :title "Boox NeoReader highlights"
    :intro "Bring the highlights and notes you made on a Boox e-reader into the EPUB or PDF you read them in. Google Drive carries the file; nothing is written back to it or to the device."
    :steps [{:label "Export on the device"
             :detail "In NeoReader, export the book's annotations as HTML. The file lands in onyx / your device name / Read in your Google Drive."
             :commands #{}}
            {:label "Extract the text first (PDF only)"
             :detail "A PDF's pages hold no text until you Scan Page or Copy text on them, and a page with no text can hold no highlight. Copy text matches your Boox export most closely, because both read the PDF's own text layer."
             :commands #{}}
            {:label "Start the sync"
             :detail "Library → Documents → the document's ⋯ menu → Sync with NeoReader. The first run asks Google for read access to one file, then opens Google's own file picker."
             :commands #{:neoreader-sync}}
            {:label "Pick the export"
             :detail "Go to onyx → your device → Read and pick the book's HTML file. FreeMemo remembers it, so every later sync is one click with no picker."
             :commands #{}}
            {:label "Read the result"
             :detail "Each highlighted passage is tinted orange in its chapter or page. A passage you annotated also carries a [1] marker, and that chapter or page ends with a numbered USER NOTES list."
             :commands #{}}
            {:label "Check what did not place"
             :detail "A highlight FreeMemo cannot find in the text is listed under UNPLACED HIGHLIGHTS — on the document itself for an EPUB, on page 1 for a PDF. An unscanned page is the usual cause on a PDF; editing a sentence after you highlighted it is the usual cause on either."
             :commands #{}}
            {:label "Sync again any time"
             :detail "Every sync rebuilds from the export. A highlight you deleted on the device loses its mark, and your own edits inside a USER NOTES list do not survive."
             :commands #{}}]}
   {:slug "commit-walkthrough"
    :title "Walking through commits"
    :intro "Learn what changed between two commits of an imported repository. FreeMemo groups the changes by concept, and the tutor teaches one concept per step, from what you already know."
    :steps [{:label "What you need"
             :detail "A repository imported from a forge, with Import → More sources → Code. A repository uploaded as a .zip has no history, so it has no walkthrough. Preparing a walkthrough calls the AI model, and the tutor calls it for each answer. Settings → Usage → AI costs lists the preparation as Walkthrough concepts and Walkthrough grouping, and the recording of known ideas as Walkthrough linking."
             :commands #{}}
            {:label "Open the picker"
             :detail "On the repository's home, choose Walk through commits…, or run Walk through commits… from the command palette. To learn one commit, open its commit topic and choose Walk through this commit. FreeMemo first fetches the repository, so the lists show its newest commits. While it reads, the picker says Reading the repository's history…. The lists hold the newest 500 commits of the branch you imported."
             :commands #{:start-walkthrough}}
            {:label "Choose the commits"
             :detail "In First commit to learn and Last commit to learn, type part of a short commit id or of a commit message, and pick a commit. Both commits are taught. Choose the same commit in both fields to learn that commit alone. After you pick one field, the other offers only the commits that make a valid range. A line under the fields says how many commits the walkthrough teaches. Then choose Start walkthrough."
             :commands #{}}
            {:label "Know the limits"
             :detail "A walkthrough teaches at most 50 commits. Both commits must be on one branch line: the first commit must be on the line of first parents that leads to the last one. So a commit that is only on a merged side branch cannot be the first. The repository's very first commit cannot be the first commit either, because nothing comes before it to compare with. Choose the commit after it."
             :commands #{}}
            {:label "Wait for the preparation"
             :detail "The walkthrough opens at once and shows what it does: reading diffs, finding concepts, grouping changes, saving. First the model names the concepts the commits add, and orders them so each concept comes after the concepts it needs. Then it puts each change into one concept. You can leave the page. The Activity panel shows the work, and Cancel stops it. A model call that already ran keeps its charge."
             :commands #{:start-walkthrough :prepare-walkthrough :cancel-walkthrough}}
            {:label "When the preparation fails"
             :detail "The walkthrough says why it could not be prepared. Try again prepares the same range again. A failed preparation stores no grouping, so nothing is lost. A cancelled walkthrough also shows as failed, with the reason Cancelled, and Try again starts it again."
             :commands #{:restart-walkthrough}}
            {:label "Open a walkthrough again"
             :detail "The repository's home lists its walkthroughs, the most recently progressed first. Each row shows the walkthrough's name, its first and last commit, its number of commits, how many steps you understood out of all of them, and when you last progressed. Select a row to open it at the step where you stopped."
             :commands #{}}
            {:label "Name a walkthrough"
             :detail "A walkthrough is named after its first step until you name it. Choose Rename on its row, or Rename beside its name on the walkthrough screen. Leave the name empty to go back to the step name. A name holds at most 120 characters. The browser tab shows the name too."
             :commands #{:rename-walkthrough}}
            {:label "Delete a walkthrough"
             :detail "Choose Delete on its row, then Delete to confirm. Its steps, your progress and your step fixes are deleted. Pins and known ideas stay. A walkthrough that is still preparing has no Delete: cancel it first."
             :commands #{:delete-walkthrough}}
            {:label "Find your way on the screen"
             :detail "The walkthrough replaces the repository's document column, and the Assistant panel stays on the right. ← Repository goes back to the repository. The header shows the walkthrough's name, the repository, the first and the last commit, and the number of commits. The step bar under it holds Start and one chip for each step, in teaching order. Select a chip to open its step. A step you finished shows ✓."
             :commands #{}}
            {:label "Other changes and Noise"
             :detail "When they exist, two steps come last. Other changes holds the changes that fit no concept. Noise holds the changes nobody reads line by line. These are files that the repository's .gitattributes marks as vendored or generated, and changes of more than 1500 lines. Noise shows in italics, and you can skip it."
             :commands #{}}
            {:label "Test what you know first"
             :detail "Start lists the steps ahead, each with a short summary. Choose Test what I know about the starting commit. This starts a new chat in the Assistant panel, so no earlier conversation is in it. The tutor asks about the code just before your first commit, one question at a time. It corrects a wrong belief briefly and does not teach the changes yet. An idea you show you know is recorded as known, with that commit. Later steps read it, so the tutor skips what you know. It also tells you when a step changes an idea you learned before. Choose I understand to go to step 1."
             :commands #{:ask-walkthrough-tutor :apply-walkthrough-known}}
            {:label "Read a step"
             :detail "A step shows its name and summary, then its main file whole. The main file is a Markdown file when the step changes one. Otherwise it is the file with the most changed lines. The file shows as it was after the last commit in which this step changes it. A file the step deletes shows as it was just before the deletion. Markdown is rendered, as in the code pane."
             :commands #{}}
            {:label "Read the colors"
             :detail "The colors show which step wrote each line. In code, a line this step wrote has a blue tint and a blue bar. A line another step wrote has a gray bar, and an unchanged line is dim. In Markdown, a block takes the color of its strongest change, a block another step wrote is outlined, and an unchanged block is gray. The colors do not mark lines as added or removed. A removed line is not in the file, so the canvas cannot show it. The file list counts removed lines, and the tutor sees them. When FreeMemo cannot tell which change wrote each line, the file shows plainly, with a note."
             :commands #{}}
            {:label "Since previous step and Since A"
             :detail "A is the code just before your first commit. Since previous step, the default, colors only this step's changes and marks every other step's. Since A also colors the changes of earlier steps, in a lighter tint. Use it to see everything that changed from A up to this step. The toggle changes the colors only."
             :commands #{}}
            {:label "Open folded code"
             :detail "In a code file longer than 150 lines, long runs of unchanged lines fold into a bar such as ⋯ 120 unchanged lines. Select the bar to open the run, and ⋯ hide unchanged lines to close it. A few lines next to each change stay open. Long lines wrap, so no line runs past the right edge."
             :commands #{}}
            {:label "Use the file list"
             :detail "Files, at the right, lists every file the step changes, with its number of changes, and deleted when the step deletes it. Select a file name to show that file. Changes lists each change in the file: its commit, the lines it adds and removes, and where in the file it is."
             :commands #{}}
            {:label "Move on"
             :detail "Choose I understand when the step is clear, or run I understand this step from the command palette. The step gets ✓, and the next step opens. Select any chip to go back. The walkthrough remembers your step, so closing it and coming back opens where you stopped."
             :commands #{:walkthrough-step}}
            {:label "Fix the grouping"
             :detail "The model sometimes groups a change wrongly, and three fixes are available. To move one change, open its file's Changes in the file list and pick another step: the change moves at once. To rename a step, edit its name beside I understand and choose Rename. Two steps cannot have the same name. To merge, use Merge this step into… when the model split one idea into two steps. Pick the other step, then choose Merge to confirm. This step's changes move into that step, this step is deleted, and the later steps are numbered again. A merge cannot be undone."
             :commands #{:move-walkthrough-hunk :rename-walkthrough-concept :merge-walkthrough-concepts}}
            {:label "Ask about a step"
             :detail "Ask the tutor in the Assistant panel. It knows which step and which file you are reading. It reads the step's changes, the file, and why the commits changed it. The why is the commit messages and their changes to plan files, which are Markdown files in a plans/ directory. It teaches the concept, cites a change or a passage for each claim, and asks you questions to check your understanding."
             :commands #{}}
            {:label "Follow a citation"
             :detail "Point at a citation in an answer to light the cited file in the file list. Select the citation to show that file and point at the cited passage."
             :commands #{}}
            {:label "Pin questions on a step"
             :detail "Select a passage in the file and choose Ask, as on any repository file. Type a question, or press Enter to say you do not understand the passage. Pins lists them, and Ask sends the open pins to the tutor together. A pin keeps the commit it was made on. When that commit is imported, the pin also shows on the file's topic there."
             :commands #{}}]}
   {:slug "ai-assistant"
    :title "Asking the AI assistant"
    :intro "Work a page through with an assistant that either answers you or questions you — your choice."
    :steps [{:label "Open it"
             :detail "Open the Assistant panel on the right. ＋ New clears the panel, and your first message starts a saved conversation about the current document. The panel reopens the chat you last had open."
             :commands #{:toggle-assistant-pane :new-assistant-chat}}
            {:label "Find, rename, pin and delete chats"
             :detail "Select the history button left of the chat title. Chats are grouped as Pinned, Today, Previous 7 days and Older, and the search field filters them by title. Select a title, or press F2 on a row, to rename it. The ⋯ menu or a right-click pins, renames or deletes a chat. A deleted chat can be restored with Undo for 12 hours."
             :commands #{:show-assistant-history :edit-assistant-chat-title
                         :delete-active-assistant-chat :rename-assistant-chat
                         :set-assistant-chat-pinned :delete-assistant-chat}}
            {:label "Set the panel height"
             :detail "Settings → AI Features → Assistant panel height. Content area stops above the card table. Full workspace extends beside it."
             :commands #{}}
            {:label "Pick a mode"
             :detail "Use the Mode dropdown below the composer. General answers directly. Tutor asks guiding questions. Socratic uses a fixed learning structure. Each chat keeps its mode."
             :commands #{}}
            {:label "Ask about the page"
             :detail "Type a question about what you're reading. Every mode is grounded in the page in front of you. Markdown and math render inline."
             :commands #{}}
            {:label "Reference another document"
             :detail "Type @ in the composer and pick a document. The menu stays above the composer, and the selected document becomes a removable chip."
             :commands #{}}
            {:label "Check source evidence"
             :detail "Select an evidence link to read its retained excerpt. Evidence from another document includes Open document in new tab."
             :commands #{}}
            {:label "Pin questions on a repository file"
             :detail "On a file from a repository, in the rendered view of a Markdown file or in the source of any file, select a passage and choose Ask. Type a question and press Enter, or press Enter with no text to say you do not understand the passage. Each pin keeps its number. Pins in the file list, under the code pane's Pins button, can be closed, reopened or deleted. A pin whose passage changed in a newer commit shows there as stale."
             :commands #{:ask-about-selection :save-question-pin :delete-question-pin}}
            {:label "Ask your pins together"
             :detail "Choose Ask N pins in the code pane or in the composer. The tutor reads every open pin together and replies with a map of the ideas they depend on, then asks its first question about the idea marked Start here. Point at a citation to light up its passage in the file."
             :commands #{:ask-question-pins :apply-question-map}}
            {:label "Work the map"
             :detail "Each idea in the map offers I know it and Teach me. I know it is remembered for every document you read, and Undo removes it. When your answers show you understand an idea, the tutor marks it learned and closes its pins."
             :commands #{:teach-question-root :mark-question-root-known}}
            {:label "Pick its model"
             :detail "Use the Model dropdown below the composer to set the model for this document. Leave it on your default otherwise."
             :commands #{}}
            {:label "Manage an answer"
             :detail "Answers stream into the saved chat. Stop an active answer. Answer details shows its request, timing, provider, and sources. Failed answers offer Retry."
             :commands #{}}]}
   {:slug "reading-schedule"
    :title "Planning a reading schedule"
    :intro "Plan a document page range to a target date, with reading and Anki review work in one forecast."
    :steps [{:label "Create a document schedule"
             :detail "Schedule → New schedule. Pick a document, target date, page range, and daily review capacity. A blank last page means the document end."
             :commands #{}}
            {:label "Read the dashboard"
             :detail "Curriculum schedules and document schedules appear separately. A curriculum card shows its remaining study minutes. Select a document schedule to open its forecast."
             :commands #{}}
            {:label "Follow the reading pill"
             :detail "The Viewer toolbar shows today's page quota and deadline for the schedule that covers the current page. Hover to see your pace."
             :commands #{}}
            {:label "Inspect the plan"
             :detail "Study plan is the default view. It lists daily pages and reviews through card maturation. Switch to Pages for each page's state."
             :commands #{}}
            {:label "Handle a past target"
             :detail "After the target passes, the report shows the overdue time, page coverage, and expected card maturation date. You can move the target."
             :commands #{}}
            {:label "Archive or restore"
             :detail "Archive removes a schedule from active work and its daily load. Open Archived in the schedule list to restore it."
             :commands #{}}]}
   {:slug "building-a-curriculum"
    :title "Building a curriculum"
    :intro "Arrange imported material into an ordered study path with blocks, schedule controls, and progress gates."
    :steps [{:label "Create it"
             :detail "Curriculum → New curriculum. Name it and set a target date, daily study minutes, study days, or a useful combination."
             :commands #{}}
            {:label "Add blocks and materials"
             :detail "Add blocks in study order. In an unlocked block, Add material searches all imported sources. Use the type filters to narrow the list."
             :commands #{}}
            {:label "Order the work"
             :detail "Use the arrow controls to move blocks and materials. Edit a material's minute estimate when the default is wrong."
             :commands #{}}
            {:label "Set the focus"
             :detail "Make one available block the focus. Later blocks unlock after you pass the prior block exam. The override can skip that exam."
             :commands #{}}
            {:label "Track readiness"
             :detail "Mark a material studied, build its Anki cards, and pass its exam. The curriculum schedule shows the next tasks and estimated finish."
             :commands #{}}
            {:label "Edit or delete"
             :detail "Edit schedule controls from the curriculum header. Use the pencil to rename a block. Delete curriculum hides its blocks and exams for 12 hours. Undo restores them. Imported documents stay."
             :commands #{:curriculum-change :rename-block :delete-curriculum}}]}
   {:slug "curriculum-exams"
    :title "Taking curriculum exams"
    :intro "Generate evidence-bound material and block exams, save answer drafts, and keep a reviewable grade history."
    :steps [{:label "Generate an exam"
             :detail "Select Generate exam on a material or block. Generation shows progress and survives page changes. Use Activity to monitor or cancel it."
             :commands #{}}
            {:label "Review and approve"
             :detail "The exam route shows the blueprint, evidence, and questions. Use Edit to change a draft. Approve freezes the version used by later sittings."
             :commands #{}}
            {:label "Take or resume it"
             :detail "Take exam opens five answer fields. Draft answers save when you leave a field. Reloading the sitting route restores the same exam and elapsed time."
             :commands #{:save-exam-answer-draft}}
            {:label "Grade it"
             :detail "Submit freezes the answers and requests an AI grade. Use Regrade with AI when needed. A failed result can create a targeted retry exam."
             :commands #{}}
            {:label "Cancel or archive"
             :detail "Cancel this sitting closes an unsubmitted attempt without a grade. Archive removes a cancelled sitting from the list. Submitted sittings remain in history."
             :commands #{:cancel-exam-sitting :archive-exam-sitting}}
            {:label "Understand credits"
             :detail "Paid actions show a credit notice before use. Completed forms and grades show their recorded charge. AI Costs lists every returned model charge."
             :commands #{}}]}
   {:slug "making-a-deck"
    :title "Making a deck"
    :intro "Generate flashcards from the page you're reading, then curate them."
    :steps [{:label "Generate ({generate})"
             :detail "Pick Basic or Cloze, a card count, and whether to include previous-page context, then generate from the current page. If the document has approved knowledge-graph facts, cards are built from those facts instead of the raw text."
             :commands #{:generate}}
            {:label "Generate with a prompt"
             :detail "Add a custom instruction (e.g. \"focus on definitions\") — recent prompts autocomplete."
             :commands #{:gen-prompt}}
            {:label "Compare models"
             :detail "Run the same content through two or more card models side by side and keep the set you prefer; each model is a separate, billed generation."
             :commands #{:compare-card-gen}}
            {:label "Curate"
             :detail "Add a card from a text selection ({add-new}) — the passage is marked in the editor — edit any card's question/answer or cloze, or delete the weak ones."
             :commands #{:add-new :add-card :edit-card :delete-card}}]}
   {:slug "image-occlusion-cards"
    :title "Image-occlusion cards"
    :intro "Hide parts of an image and quiz yourself on what's under the mask."
    :steps [{:label "Open the editor"
             :detail "Right-click an image in the topic editor → Image Occlusion…"
             :commands #{}}
            {:label "Draw masks"
             :detail "In the Masks Editor tab, pick Draw (R) and drag on the image to add a mask. Pick Select (S) to adjust: click a mask, drag to move, handles to resize, Delete to remove."
             :commands #{}}
            {:label "Group masks (optional)"
             :detail "With Select (S), Shift-click or drag a box over several masks, then Group (G) — grouped masks are revealed together as ONE card. Ungroup (U) splits them again. The footer shows \"N masks → M cards\"."
             :commands #{}}
            {:label "Add fields (optional)"
             :detail "The Fields tab holds Header, Footer, Remarks, Sources, and two Extra fields."
             :commands #{}}
            {:label "Create the cards"
             :detail "Hide One, Guess One makes one card per mask group; Hide All, Guess One hides them all on every card. Push to Anki as image-occlusion notes."
             :commands #{:add-occlusion}}]}
   {:slug "syncing-to-anki"
    :title "Syncing to Anki"
    :intro "Push curated cards into your own Anki collection over AnkiConnect."
    :steps [{:label "Set up AnkiConnect"
             :detail "Settings → Anki Sync walks you through it: install Anki and the AnkiConnect add-on, press Connect, and choose Yes when Anki asks about FreeMemo."
             :commands #{}}
            {:label "Sync ({anki-sync})"
             :detail "Choose the deck, tags and a custom header, then sync. FreeMemo owns the Basic and Cloze note types, so there's nothing to map."
             :commands #{:anki-sync}}
            {:label "Sync now ({quick-sync})"
             :detail "The same run with your saved settings, in the background — no modal."
             :commands #{:quick-sync}}
            {:label "What a run does"
             :detail "Each card moves whichever way it changed: edited here it goes to Anki, edited in Anki it comes back, deleted on one side it goes from the other. There is no direction to pick."
             :commands #{}}
            {:label "Conflicts"
             :detail "A card edited on BOTH sides is left untouched and reported. Filter Library › Cards by Conflict, then choose Keep FreeMemo or Keep Anki."
             :commands #{}}]}
   {:slug "reviewing"
    :title "Reviewing (Learn)"
    :intro "Study what's due on a spaced-repetition schedule."
    :steps [{:label "Start Learning"
             :detail "The Learn tab opens the day's due queue as a single-file session."
             :commands #{:start-learn-session}}
            {:label "Order"
             :detail "The due date decides membership; priority (0 = highest) orders the queue; ties shuffle stably for the day."
             :commands #{}}
            {:label "Advance or postpone"
             :detail "Move to the next item, or push a topic's due date out by a number of days."
             :commands #{:advance-topic :postpone-topic}}
            {:label "Track"
             :detail "The Learn dashboard shows due/studied counts and streaks; Repetition History shows a topic's past reviews."
             :commands #{}}]}
   {:slug "knowledge-graph"
    :title "Building a knowledge graph"
    :intro "Distill a document into a fact graph you can curate and quiz against."
    :steps [{:label "Distill"
             :detail "Knowledge → Documents → Distill on a document. It runs in the background with a spinner and a Cancel; Re-distill re-runs it. Facts land approved — you curate by exception, not one by one."
             :commands #{:distill}}
            {:label "Generate questions"
             :detail "Questions turns each fact into an atomic question; both actions are also in the {toggle-palette} palette."
             :commands #{:generate-questions}}
            {:label "Fix the facts"
             :detail "Facts (N) opens the fact table — edit or relink an object, or × to reject a fact."
             :commands #{}}
            {:label "Curate entities"
             :detail "Entities: Rename one, Merge… two that name the same thing (irreversible), or Synthesize questions for one."
             :commands #{}}
            {:label "Curate questions"
             :detail "Questions: click a row to edit its question and reference answer, or × to reject it."
             :commands #{}}]}
   {:slug "quiz"
    :title "Testing yourself — Quiz & Timed Quiz"
    :intro "Test recall against the fact graph, graded by the model."
    :steps [{:label "Set it up"
             :detail "Quiz tab → toggle Quiz or Timed Quiz, check the source documents, set Questions (and Minutes for a timed quiz), then Start."
             :commands #{:start-quiz :start-exam}}
            {:label "Quiz"
             :detail "Untimed. Submit an answer for an instant verdict — ✓ Correct, ◐ Partial, or ✗ Incorrect — with an explanation and the facts you missed."
             :commands #{}}
            {:label "Timed Quiz"
             :detail "Timed and forward-only. Answers are saved as you go and graded only when you submit, or when the timer runs out."
             :commands #{}}
            {:label "Review past sessions"
             :detail "History lists your previous quizzes and timed quizzes with scores."
             :commands #{:quiz-history}}]}
   {:slug "finding-and-organizing"
    :title "Finding & organizing"
    :intro "Search, browse, reshape, and review your growing library."
    :steps [{:label "Search"
             :detail "The Search tab finds topics by their text, Fuzzy or Exact. Card text is filtered separately in Library → Cards."
             :commands #{:search}}
            {:label "Browse the Library"
             :detail "Library toggles between Documents (the knowledge tree) and Cards; Expand All / Collapse All opens the tree. Each document's Done and Synced columns show its progress."
             :commands #{}}
            {:label "Use the Library on a phone"
             :detail "Filters stack above the list, document progress uses a compact row, and the row menu provides a large action target."
             :commands #{}}
            {:label "Re-nest topics"
             :detail "Use Hierarchy, the default left-panel tab. Drag a row by its ⠿ grip to a new parent. Switch to Pins for saved images. Press Esc to cancel a move."
             :commands #{:move-topic :show-hierarchy :show-pins}}
            {:label "Review a subset"
             :detail "In Library → Documents, a document's ⋯ menu → Review studies just that topic's children."
             :commands #{:open-subset-review}}]}
   {:slug "keyboard"
    :title "Control FreeMemo with the keyboard"
    :intro "Use focus keys, the command palette, and contextual controls to operate FreeMemo without a pointer."
    :steps [{:label "Start with focus"
             :detail "Press Tab to show the Skip to content link. Press Enter to bypass the sidebar."
             :commands #{}}
            {:label "Move between controls"
             :detail "Press Tab to move forward. Press Shift+Tab to move backward. A visible ring shows the focused control."
             :commands #{}}
            {:label "Activate controls"
             :detail "Press Enter or Space to activate a focused button, row, result, card, review, exam, transcript segment, or heatmap day."
             :commands #{}}
            {:label "Open any tab"
             :detail "Open {toggle-palette}, type “Go to”, select a destination with the arrow keys, and press Enter."
             :commands #{:go-home :go-learn :go-schedule :go-library :go-knowledge :go-quiz :go-search :go-import :go-settings :go-curriculum :go-help :go-graph}}
            {:label "Use the command palette ({toggle-palette})"
             :detail "Type to filter actions. Use ↑ and ↓ or Ctrl+n and Ctrl+p. Press Enter to run an action. Press Escape to close."
             :commands #{:toggle-palette}}
            {:label "Read documents"
             :detail "{prev-page} and {next-page} change pages. {scan} scans a page. {done} marks it done."
             :commands #{}}
            {:label "Make cards"
             :detail "{extract} extracts selected text. {generate} generates cards. {add-new} opens a new card. {undo-newest} undoes the latest action."
             :commands #{:undo-newest}}
            {:label "Sync Anki"
             :detail "{anki-sync} opens Anki sync. {quick-sync} runs sync with the saved settings."
             :commands #{}}
            {:label "Use tables and lists"
             :detail "Focus a row and press Enter or Space. Focus a sortable header and press Enter or Space."
             :commands #{}}
            {:label "Use the topic tree"
             :detail "Focus an arrow and press Enter or Space to expand it. Focus the row to open the topic."
             :commands #{}}
            {:label "Move a topic"
             :detail "Activate its ⠿ grip. Tab to a destination grip and activate it. Press Escape to cancel."
             :commands #{}}
            {:label "Use the graph"
             :detail "Open Filters and use Find a concept. Type a name, use the arrow keys, and press Enter."
             :commands #{}}
            {:label "Use rich-text editors"
             :detail "In the document editor, press Tab to indent a line and Shift+Tab to outdent it. Press Escape, then Tab, to leave the editor. In a card field, Tab moves to the next field. Select text and use the format menu's indent buttons to indent there. In a dialog, press Escape a second time to close it."
             :commands #{}}
            {:label "Use dialogs"
             :detail "Press Tab or Shift+Tab to move inside a dialog. Press Escape to close it and restore focus."
             :commands #{}}
            {:label "Resize panes"
             :detail "Tab to a separator. Use the arrow keys to resize its adjacent panes."
             :commands #{}}
            {:label "Spatial editors"
             :detail "Score regions and image-occlusion masks require precise pointer input. All other documented workflows support keyboard control."
             :commands #{}}]}])

(def tab->slug
  "Routed tab → the Help section that explains it, for the rail's Help for
   this page button (plans/searchable-help.md 1.5, 5.1). A tab with no
   section maps to `[:none reason]`. help_content_test asserts every key of
   `page-title-format/tab-labels` is here, so a new tab cannot go unmapped."
  {:home       "getting-around"
   :viewer     "studying-a-pdf"
   :library    "finding-and-organizing"
   :learn      "reviewing"
   :schedule   "reading-schedule"
   :curriculum "building-a-curriculum"
   :quiz       "quiz"
   :search     "finding-and-organizing"
   :knowledge  "knowledge-graph"
   :import     "importing-content"
   :graph      [:none "No Graph section yet."]
   :help       [:none "This is the Help page."]
   :settings   [:none "No Settings section yet."]
   :credits    [:none "No Credits section yet."]})

(def kind->slug
  "Topic kind → the Help section that explains it. One viewer route serves
   every kind, so Help for this page reads the kind of the topic in view
   (plans/help-for-viewer-topic-kind.md 1.1). help_content_test asserts its
   keys equal freememo.topic-kinds/all."
  {"pdf"            "studying-a-pdf"
   "page"           "studying-a-pdf"
   "basic"          "studying-a-pdf"
   "epub"           "import-file"
   "markdown"       "import-file"
   "web"            "import-link"
   "wikipedia"      "import-link"
   "supermemo"      "import-supermemo"
   "video"          "import-video"
   "video-playlist" "import-video"
   "audio"          "import-audio"
   "score"          "score-cards"
   "poem"           "import-poem"
   "code"           "learning-a-codebase"
   "repo"           "learning-a-codebase"
   "commit"         "learning-a-codebase"
   "dir"            "learning-a-codebase"})

(def undocumented
  "Registry ids no step documents, each with its reason. `:pending` means Help
   owes the command a step. help_content_test caps the number of `:pending`
   entries, and the cap only goes down (plans/searchable-help.md 6.4)."
  {:credits-granted           "Bookkeeping after a payment. No user action runs it."
   :debit-credits             "Bookkeeping after a charge. No user action runs it."
   :open-topic                "Opens a topic from a link. It is not a step a user takes."
   :save-video-position       "Saved on pause and on leaving. No user action runs it."
   :save-reading-position     "Saved on a scroll pause and on leaving. No user action runs it."
   :mark-assistant-chat-opened "Saved when a chat opens. No user action runs it."
   :set-setting               "The write behind every Settings control. The Settings page is its UI."
   :add-pin                   :pending
   :apply-code-language       :pending
   :bulk-delete-cards         :pending
   :bulk-reject-facts         :pending
   :card-history              :pending
   :compare-ocr               :pending
   :copy-all                  :pending
   :copy-card                 :pending
   :delete-document           :pending
   :dismiss                   :pending
   :find-next-marked-card     :pending
   :find-next-remarked-card   :pending
   :find-next-suspended-card  :pending
   :flag-question             :pending
   :insert-audio              :pending
   :open-activity             :pending
   :open-undo-history         :pending
   :parse-questions           :pending
   :push-biblio               :pending
   :refetch-biblio            :pending
   :remove-pin                :pending
   :rename-topic              :pending
   :reset-dedup-thresholds    :pending
   :restore                   :pending
   :save-biblio               :pending
   :set-page-done             :pending
   :set-pdf-done              :pending
   :show-getting-started      :pending
   :suspend-question          :pending
   :sweep-duplicates          :pending
   :sweep-duplicates-all      :pending
   :toggle-card-table-view    :pending
   :toggle-focus-reader       :pending
   :toggle-left-pane          :pending
   :toggle-pin-placement      :pending
   :undismiss                 :pending
   :undo-entry                :pending
   :update-occlusion          :pending})

;; ── Lookups ────────────────────────────────────────────────────────────────

(def ^:private binds
  "command-id → registry bind string, for chord-token resolution."
  (into {} (commands/bindings)))

(defn resolve-chords
  "Replace `{command-id}` tokens in `s` with the platform display chord
   (commands/display-chord) — \"⌘S\" on macOS, \"Ctrl+S\" elsewhere.
   Unknown or unbound ids stay as literal text."
  [s]
  (str/replace s #"\{([a-z-]+)\}"
    (fn [[whole id]]
      (or (commands/display-chord (keyword id) (get binds (keyword id)))
          whole))))

(defn resolved-text
  "The text a user reads for a step: label and detail, chord tokens resolved.
   Display and search both read this (plans/searchable-help.md 1.4)."
  [{:keys [label detail]}]
  (str (resolve-chords label) " " (resolve-chords detail)))

(defn section-by-slug
  "The section whose `:slug` is `slug`, or nil."
  [slug]
  (some #(when (= slug (:slug %)) %) sections))

(defn slug-for-tab
  "The section slug for routed tab `tab`, or nil when the tab has no section.
   A tab missing from `tab->slug` also gives nil; the test makes that a failure."
  [tab]
  (let [v (get tab->slug tab)]
    (when (string? v) v)))

(defn slug-for-viewer
  "The Help section for the viewer in `viewer-mode`, showing a topic of `kind`.

   Pre : `viewer-mode` is :learn-session, :subset-review or :topic. `kind` is a
         topic kind or nil.
   Post: a session gets its own section, whatever item it shows. A topic gets
         its kind's section, or the viewer tab's when the kind is nil or
         unknown."
  [viewer-mode kind]
  (case viewer-mode
    :learn-session "reviewing"
    :subset-review "finding-and-organizing"
    (or (get kind->slug kind) (slug-for-tab :viewer))))

(def ^:private command->slug
  "Registry id → the slug of the FIRST section, in reading order, with a step
   that documents it. The keyboard section repeats ids the specific sections
   own, and it comes last, so the specific section wins."
  (reduce (fn [m {:keys [slug steps]}]
            (reduce (fn [m id] (if (contains? m id) m (assoc m id slug)))
              m (mapcat :commands steps)))
    {} sections))

(defn slug-for-command
  "The slug of the section that documents registry id `id`, or nil."
  [id]
  (get command->slug id))

(defn- contains-ci?
  "True when `text` contains `query`, ignoring case. Pre: query is not blank."
  [text query]
  (str/includes? (str/lower-case text) (str/lower-case query)))

(defn filter-sections
  "The sections a Help-page query leaves, in reading order
   (plans/searchable-help.md 3.1, 3.2).

   Matching is a case-insensitive substring test on resolved text — never
   command-palette/match-score, whose subsequence tier matches almost any long
   detail. A section whose title or intro matches keeps every step. Otherwise
   it keeps the steps that match, and a section with none left is dropped.

   Post: a blank query returns `sections` unchanged."
  [query]
  (let [q (str/trim (or query ""))]
    (if (str/blank? q)
      sections
      (keep (fn [{:keys [title intro steps] :as section}]
              (if (or (contains-ci? title q) (contains-ci? (resolve-chords intro) q))
                section
                (let [hits (filterv #(contains-ci? (resolved-text %) q) steps)]
                  (when (seq hits) (assoc section :steps hits)))))
        sections))))

(defn step-label-hit?
  "True when some step label of `section`, resolved, contains `query`.
   The palette's second tier of help-row matching (D10). Pre: query is not
   blank — an empty query matches every title already."
  [section query]
  (boolean (some #(contains-ci? (resolve-chords (:label %)) query) (:steps section))))
