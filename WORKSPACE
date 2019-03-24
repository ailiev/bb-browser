workspace(name = "com_github_buildbarn_bb_browser")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")

http_archive(
    name = "bazel_toolchains",
    sha256 = "109a99384f9d08f9e75136d218ebaebc68cc810c56897aea2224c57932052d30",
    strip_prefix = "bazel-toolchains-94d31935a2c94fe7e7c7379a0f3393e181928ff7",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-toolchains/archive/94d31935a2c94fe7e7c7379a0f3393e181928ff7.tar.gz",
        "https://github.com/bazelbuild/bazel-toolchains/archive/94d31935a2c94fe7e7c7379a0f3393e181928ff7.tar.gz",
    ],
)

http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "aed1c249d4ec8f703edddf35cbe9dfaca0b5f5ea6e4cd9e83e99f3b0d1136c3d",
    strip_prefix = "rules_docker-0.7.0",
    urls = ["https://github.com/bazelbuild/rules_docker/archive/v0.7.0.tar.gz"],
)

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "6776d68ebb897625dead17ae510eac3d5f6342367327875210df44dbe2aeeb19",
    urls = ["https://github.com/bazelbuild/rules_go/releases/download/0.17.1/rules_go-0.17.1.tar.gz"],
)

http_archive(
    name = "bazel_gazelle",
    sha256 = "3c681998538231a2d24d0c07ed5a7658cb72bfb5fd4bf9911157c0e9ac6a2687",
    urls = ["https://github.com/bazelbuild/bazel-gazelle/releases/download/0.17.0/bazel-gazelle-0.17.0.tar.gz"],
)

http_archive(
    name = "com_github_bazelbuild_bazel",
    patches = ["//:patches/com_github_bazelbuild_bazel/build_event_stream.diff"],
    sha256 = "6860a226c8123770b122189636fb0c156c6e5c9027b5b245ac3b2315b7b55641",
    urls = ["https://github.com/bazelbuild/bazel/releases/download/0.22.0/bazel-0.22.0-dist.zip"],
)

http_archive(
    name = "com_github_edschouten_rules_elm",
    sha256 = "f6f1bd1b2e03b2d6b59781e48115126ba14fd8792f4c08229f706c13461beae7",
    strip_prefix = "rules_elm-0.1",
    urls = ["https://github.com/EdSchouten/rules_elm/archive/v0.1.tar.gz"],
)

http_archive(
    name = "com_github_twbs_bootstrap",
    build_file_content = """exports_files(["css/bootstrap.min.css", "js/bootstrap.min.js"])""",
    sha256 = "888ffd30b7e192381e2f6a948ca04669fdcc2ccc2ba016de00d38c8e30793323",
    strip_prefix = "bootstrap-4.3.1-dist",
    urls = ["https://github.com/twbs/bootstrap/releases/download/v4.3.1/bootstrap-4.3.1-dist.zip"],
)

http_file(
    name = "com_jquery_jquery",
    sha256 = "0497a8d2a9bde7db8c0466fae73e347a3258192811ed1108e3e096d5f34ac0e8",
    urls = ["https://code.jquery.com/jquery-3.4.0.min.js"],
)

load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

git_repository(
    name = "com_github_buildbarn_bb_storage",
    commit = "afbc0eef9ad8065bb617df8682fe876e553f571b",
    remote = "https://github.com/buildbarn/bb-storage.git",
)

load("@io_bazel_rules_docker//repositories:repositories.bzl", container_repositories = "repositories")

container_repositories()

load("@com_github_edschouten_rules_elm//elm:deps.bzl", "elm_register_toolchains")

elm_register_toolchains()

load("@com_github_edschouten_rules_elm//repository:def.bzl", "elm_repository")

elm_repository(
    name = "elm_package_elm_browser",
    sha256 = "c4c3cb453bfe2dfc3a31f1760688d96e82d0bf091e00c99faadb8ed495dff5aa",
    strip_prefix = "browser-1.0.0",
    urls = ["https://github.com/elm/browser/archive/1.0.0.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_core",
    sha256 = "9cfa20b6468b8bfb4f02c6652f43de1dd1c58b328060830ab804964da0417982",
    strip_prefix = "core-1.0.0",
    urls = ["https://github.com/elm/core/archive/1.0.0.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_html",
    sha256 = "73b885e0a3d2f9781b1c9bbcc1ee9ac032f503f5ef46a27da3ba617cebbf6fd8",
    strip_prefix = "html-1.0.0",
    urls = ["https://github.com/elm/html/archive/1.0.0.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_json",
    sha256 = "cbba2f0ea00fc83f5781207a7de1d49f5a1ad6ed3ce578f218060b87a75310bc",
    strip_prefix = "json-1.0.0",
    urls = ["https://github.com/elm/json/archive/1.0.0.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_time",
    sha256 = "e18bca487adec67bfe4043a33b975d81527a7732377050d0421dd86d503c906d",
    strip_prefix = "time-1.0.0",
    urls = ["https://github.com/elm/time/archive/1.0.0.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_url",
    sha256 = "840e9d45d8a9bd64a7f76421a1de2518e02c7cbea7ed42efd380b4e875e9682b",
    strip_prefix = "url-1.0.0",
    urls = ["https://github.com/elm/url/archive/1.0.0.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_virtual_dom",
    sha256 = "5899564798629e91ef95238f8ba7f4d40260d18496b622469d69fc03457aa842",
    strip_prefix = "virtual-dom-1.0.0",
    urls = ["https://github.com/elm/virtual-dom/archive/1.0.0.tar.gz"],
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")

go_rules_dependencies()

go_register_toolchains()

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies", "go_repository")

gazelle_dependencies()

load("//:go_dependencies.bzl", "bb_browser_go_dependencies")

bb_browser_go_dependencies()

load("@com_github_buildbarn_bb_storage//:go_dependencies.bzl", "bb_storage_go_dependencies")

bb_storage_go_dependencies()
