test_that("`use_traits_build_skills` errors outside a database repo", {
  path <- withr::local_tempdir()
  expect_error(
    use_traits_build_skills(path = path),
    "does not look like a traits.build database repo"
  )

  # Having `data/` but not `config/traits.yml` is still not a database repo
  dir.create(file.path(path, "data"))
  expect_error(
    use_traits_build_skills(path = path),
    "does not look like a traits.build database repo"
  )
})

test_that("`use_traits_build_skills` installs the skill(s) shipped with the package", {
  path <- withr::local_tempdir()
  dir.create(file.path(path, "data"))
  dir.create(file.path(path, "config"))
  writeLines("concepts:", file.path(path, "config", "traits.yml"))

  src_root <- system.file("skills", package = "traits.build")
  skip_if(!nzchar(src_root) || length(list.dirs(src_root, recursive = FALSE)) == 0,
          "no skills shipped with this build of the package")

  skill_names <- list.dirs(src_root, full.names = FALSE, recursive = FALSE)

  expect_message(
    installed <- use_traits_build_skills(path = path),
    "Installed skill"
  )

  dest_root <- file.path(path, ".claude", "skills")
  for (skill in skill_names) {
    expect_true(dir.exists(file.path(dest_root, skill)))
    # Every file in the shipped skill is present in the installed copy
    src_files <- list.files(file.path(src_root, skill), recursive = TRUE)
    dest_files <- list.files(file.path(dest_root, skill), recursive = TRUE)
    expect_setequal(dest_files, src_files)
  }
  expect_setequal(basename(installed), skill_names)
})

test_that("`use_traits_build_skills` skips an already-installed skill unless `overwrite = TRUE`", {
  path <- withr::local_tempdir()
  dir.create(file.path(path, "data"))
  dir.create(file.path(path, "config"))
  writeLines("concepts:", file.path(path, "config", "traits.yml"))

  src_root <- system.file("skills", package = "traits.build")
  skip_if(!nzchar(src_root) || length(list.dirs(src_root, recursive = FALSE)) == 0,
          "no skills shipped with this build of the package")

  skill_names <- list.dirs(src_root, full.names = FALSE, recursive = FALSE)
  dest_root <- file.path(path, ".claude", "skills")

  expect_message(use_traits_build_skills(path = path), "Installed skill")

  # Mark the installed copy so we can tell whether a re-run touched it
  sentinel <- file.path(dest_root, skill_names[1], ".sentinel")
  writeLines("untouched", sentinel)

  expect_message(
    skipped <- use_traits_build_skills(path = path),
    "Skipped skill"
  )
  expect_length(skipped, 0)
  expect_true(file.exists(sentinel))

  expect_message(
    reinstalled <- use_traits_build_skills(path = path, overwrite = TRUE),
    "Installed skill"
  )
  expect_setequal(basename(reinstalled), skill_names)
  expect_false(file.exists(sentinel))
})
