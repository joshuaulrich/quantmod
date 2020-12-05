Release checklist for quantmod

* Create GitHub Milestone
    * Associate all included issues with milestone
    * Ensure all included issues are closed
    * Roll open issues to next milestone

* Check unit tests, code coverage, and benchmarks

* Build and test with R-devel and current release
    * Run via r-hub for Linux, Windows, Solaris
    * Run via win-builder

* Run reverse-dependency checks
    * Note failures that are not related to package, for example:
        * Required vignette builder is not installed
        * Unit tests do not conditionally use other packages
    * Notify dependent maintainers of API changes affecting their package

* Update NEWS
    * Include user-facing changes, with GitHub issue number
    * Include GitHub username for users who create issues and/or PRs

* Ensure [all CRAN checks](https://cran.r-project.org/web/checks/check_results_quantmod.html) are addressed

* Upload to CRAN
    * Add any new comments the [CRAN comments](CRAN_comments.md) file

* Post "on CRAN now"
    * Tag release
    * Close milestone
    * Announce:
        * Blog post, tweet, R-announce, etc
