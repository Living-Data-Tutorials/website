Hi! This is the repo for creating the Living Data Tutorials website.

## For contributors to the Living Data Project

### Step-by-step contribution

If you're a contributor, refer to the script `create_post.R` in the [`scripts`](/scripts) folder.

1. Create a branch with a name that follows this format: LESSON/ecological_concept (all branches that host a lesson must have the "LESSON/" key before the name of the ecological concept it refers to).

2. Open the `create_post.R` script. Edit the title, the author and the date fields and run it. This will create a *.md file you can edit as usual.

3. Edit the *.md file and knit it into a html file. Rebuild the website by typing rmarkdown::render_site().

4. Push your changes to your branch on GitHub. 

5. When your lesson is done, create a pull request to the main branch and tag @jennsunday and @JoeyBernhardt as reviewers.

## For instructors wishing to adapt our tutorials in their classroom

Download the files in the [`lessons folder`](/_lessons). You can then edit them and knit them in your own computer to adapt the content to your specific needs.
