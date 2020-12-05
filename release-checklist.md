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
    * Include comment about using `assign()`
        The calls to attach() are made in the function attachSymbols().  This
        function is a mechanism to wrap the complexity of attaching multiple
        symbols (a database of potentially thousands of individual instrument
        names) with one simple call.  Based on previous emails with CRAN
        regarding this specific attach usage, and the lack of anything beyond
        the NOTE 'finding' the call, this seems well within the scope of
        behavior required by CRAN.  Note that the name of the function doing
        this attach is an extension of the attach name itself, which makes the
        function's purpose clear. 
    * Make sure to include any necessary comments to CRAN

* Post "on CRAN now"
    * Tag release
    * Close milestone
    * Announce:
        * Blog post, tweet, R-announce, etc
