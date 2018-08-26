#[macro_use]
extern crate version_sync;

#[test]
fn test_readme_deps() {
    assert_markdown_deps_updated!("README.md");
}

#[test]
fn test_html_root_url_in_lib() {
    assert_html_root_url_updated!("src/lib.rs");
}

#[test]
fn test_html_root_url_in_main() {
    assert_html_root_url_updated!("src/main.rs");
}
