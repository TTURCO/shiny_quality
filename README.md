Vocal Type Assessor 🎙️📊
Vocal Type Assessor is an interactive R Shiny web application designed for audio analysis, transcription, and categorical annotation of .wav sound files. Built specifically for linguistic and phonetic research, it runs entirely client-side in the web browser via Shinylive and GitHub Pages, eliminating server timeouts and hosting costs.

✨ Key Features
Audio Review & Playback: Upload and review multiple .wav files sequentially with an integrated audio player.

Visualized Sound Analysis: Automatically generates high-depth Oscillograms and Spectrograms (0-8 kHz) using tuneR and seewave.

Structured Categorization: Classify audio clips into configurable types (Verbal, Non-verbal, Interjection, Mix) with mandatory explanation fields for complex categories.

Transcription & Translation: Record comments, phonetic/orthographic transcriptions, and target translations.

Auto-Save & Session Recovery: Automatically captures progress snapshots (vocal_review_snapshot.rds) allowing users to seamlessly resume sessions by re-uploading the working file set.

Universal Multi-Language Support: Fully supports special characters and diacritics across multiple languages (French, English, Turkish, Mandarin, Japanese, Filipino, etc.) with robust UTF-8 CSV exports.

Serverless Deployment: Hosted for free via GitHub Pages using WebAssembly (WASM), ensuring zero backend server limits or subscription fees.
