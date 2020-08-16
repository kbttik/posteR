#' Example package to say hello
#' @importFrom rlang abort
#' @importFrom rlang exec
#' @importFrom glue glue
#' @importFrom stringr str_replace_all
#' @importFrom magrittr %>%
"_PACKAGE"
#' htmlレポートをPOSTする
#'
#' Rmdファイルから、生成されたhtmlをPOSTする。htmlレポートがない場合は、生成を聞く。
#' @param Rmd_file character レポートへのpath
#'
#' @return
#' `post_html_from_Rmd()` returns the URL of the html page.
#'
#' @export
#' @examples post_html_from_Rmd("README.Rmd")
post_html_from_Rmd <- function(Rmd_file, interactive = NULL){

  if (is.null(interactive)) {
    interactive <- interactive()
  }

  if (!tolower(tools::file_ext(Rmd_file)) %in% c("rmd", "rmarkdown")) {
    abort(glue("{basename(Rmd_file)} is not .Rmd file!"))
  }

  if (Sys.getenv("TARGET_API") == ""){
    Sys.setenv(TARGET_API = ask_api())
  }

  title <-
    Rmd_file %>%
    stringr::str_split("/", simplify = T) %>%
    stringr::str_subset("\\.Rmd") %>%
    stringr::str_replace("\\.Rmd", "")

  tag <- c(
    Sys.getenv("USER"),
    get_git_repo()
  ) %>%
    .[. != ""] %>%
    paste(collapse = ",")

  html_path <-
    Rmd_file %>%
    stringr::str_replace("\\.Rmd", "\\.html")


  post_param <- rlang::exec(
    post_interactively,
    title = title,
    tag = tag,
    html_path = html_path
  )

  print(post_param)
}


#' htmlレポート転送用アドイン関数
#'
#' RstudioのAddinsで関数の起動ができるようにする。開いているRmdファイルを対象にする
#' @return
#' `post_html_from_Rmd()` returns the URL of the html page.
#'
#' @export
post_html_from_Rmd_addin <- function(){
  if (!rstudioapi::isAvailable()) {
    abort("This function must be called on RStudio!")
  }

  Rmd_file <- rstudioapi::getSourceEditorContext()$path
  if (identical(Rmd_file, "")) {
    # Probably "UntitledX"
    abort("Please save the .Rmd file first!")
  }

  #post_html_from_Rmd(Rmd_file, interactive = TRUE)
  post_html_from_Rmd(Rmd_file, interactive = TRUE)
}


#' uploadを行う
#'
#' エンコードもここでする
post_html <- function(title, tag, html_path){
  # POST
  result <-
    httr::POST(url = Sys.getenv("TARGET_API"),
               body = httr::upload_file(path = html_path, type = "text/html"),
               httr::add_headers(title = title %>% encoding_title(),
                                 tags = tag %>% encoding_tag()),
               encode = "raw")

#  if (!is.null(session)) {
#    shiny::stopApp()
#  }

  return(result)
}


#' タグのエンコード
#'
#' headerにタグを含ませる場合、utf-8で送れないので、base64でエンコードする
#' @param x character タグ sample_tag,テスト,test_tag または c("sample_tag","テスト","test_tag")
#'
#' @return character "sample_tag,テスト,test_tag"をbase64でエンコードした結果(サンプルは"c2FtcGxlX3RhZyzjg4bjgrnjg4gsdGVzdF90YWc=")
#'
#' @export
#' @examples
#' encoding_tag("sample_tag,テスト,test_tag")
#' encoding_tag(c("sample_ t a  g ","テスト "," test_tag"))
encoding_tag <- function(x){
  if (!is.character(x)){
    abort("Please change Tag to character.")
  }

  # make 1 string and remove space
  if (length(x) > 1){
    x <- paste(x, collapse = ",")
    x <- str_replace_all(x, "( |　)", "")
  }

  # encoding to base64
  x <- jsonlite::base64_enc(x)
  x <- str_replace_all(x, "\\n", "")

  return(x)
}


#' タイトルのエンコード
#'
#' headerにタイトルを含ませる場合、utf-8で送れないので、base64でエンコードする
#' @param x character タイトル
#'
#' @return character タイトルをbase64でエンコードした結果
#'
#' @export
#' @examples
#' encoding_title("test_title")
#' encoding_title("サンプルタイトル")
encoding_title <- function(x){
  if (!is.character(x)){
    abort("Please change Tag to character.")
  }

  # encoding to base64
  x <- jsonlite::base64_enc(x)
  x <- str_replace_all(x, "\\n", "")

  return(x)
}


#'
#'
#'
#'
get_git_repo <-function(path = "."){
  repo_name <-
    system("git remote -v", intern = TRUE)[1] %>%
    stringr::str_split("(/| )", simplify = TRUE) %>%
    stringr::str_subset("\\.git") %>%
    stringr::str_replace("\\.git", "")

  return(repo_name)
}


#'
#'
#'
#'
#'
ask_rstudio_prompt <- function(title, message, default = NULL) {
  if (!interactive()) {
    abort("Please set up environmental variables before running non-interactive session.")
  }

  if (rstudioapi::isAvailable()) {
    return(rstudioapi::showPrompt(title, message, default))
  }

  # Fallback to the readline
  readline(message)
}

ask_api <- function() ask_rstudio_prompt(title = "POST_API_SERVER", message = "URL of API you want to post")


#' POST時のshiny起動
#'
#' headerに
#'
#'
#'
#'
#'
post_interactively <- function(title, tag, html_path) {

  # This will be assigned in Shiny's server function
  #post_param <- NULL

  # Shiny UI -----------------------------------------------------------
  ui <- make_addin_ui(
    title = title,
    tag = tag,
    html_path = html_path
  )


  # Shiny Server -------------------------------------------------------
  server <- function(input, output, session) {
    done <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$confirm, {
        print("put confirm")
        post_param <- list(title = input$title,
                            tag = input$tag,
                            html_path = input$path)
        #print(post_param)
        shiny::stopApp(returnValue = post_param)
    })

  }

  viewer <- shiny::dialogViewer("Preview", width = 1200, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)

  #post_param
}


#' preview画面の作成
#'
#' タイトル/タグの修正を行えるようにする
#'
#'
#'
#'
make_addin_ui <- function(title, tag, html_path) {
  # title bar
  title_bar_button <- miniUI::miniTitleBarButton("confirm", "Publish", primary = TRUE)
  title_bar <- miniUI::gadgetTitleBar("Preview", right = title_bar_button)

  # title
  title_input <- shiny::textInput(inputId = "title", label = "Title", value = title)

  # parent report
  parent_input <- shiny::textInput(inputId = "parent", label = "Parent")

  # tag
  tag_input <- shiny::textInput(inputId = "tag", label = "Tag", value = tag)

  # html_path
  path_input <- shiny::textInput(inputId = "path", label = "Path", value = html_path)

  # Preview
  html_text_for_preview <- readr::read_file("/Users/kubotataiki/github/report-generator-with-react/report/from_report/test_from.html")
  preview_html <- shiny::HTML(html_text_for_preview)

  miniUI::miniPage(
    title_bar,
    miniUI::miniContentPanel(
      shiny::fluidRow(shiny::column(title_input, width = 6), shiny::column(parent_input, width = 6)),
      shiny::fluidRow(shiny::column(tag_input, width = 12)),
      shiny::hr(),
      shiny::div(preview_html)
    )
  )
}
