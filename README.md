# Raymond Pomponio — Astro redesign

This is a clean Astro starter for `rpomponio.github.io`.

## Local development

Requires Node.js 20+.

    npm install
    npm run dev

Then open the local URL Astro prints, normally `http://localhost:4321`.

## Deployment

The included GitHub Actions workflow builds the Astro site and deploys
`dist/` to GitHub Pages whenever `main` is updated.

For the repository `rpomponio.github.io`, the production URL is:

    https://rpomponio.github.io

## Migration strategy

Do not delete the existing Quarto site immediately. Work on a branch first,
and migrate content into `src/content/` incrementally.

The intended final content model is:

    src/content/
      science/
      adventures/
      creative/

The HTML pages in `src/pages/` are deliberately minimal placeholders. The
next step is to implement the full three-theme design and Astro content
collections.
