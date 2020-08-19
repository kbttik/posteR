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

  # get title from .Rmd
  # "tools/test_post_to_api.Rmd" to test_post_to_api
  title <-
    Rmd_file %>%
    stringr::str_split("/", simplify = T) %>%
    stringr::str_subset("\\.Rmd") %>%
    stringr::str_replace("\\.Rmd", "")

  # get tag
  # USER, git repository name
  tag <- c(
    Sys.getenv("USER"),
    get_git_repository()
  ) %>%
    .[. != ""] %>%
    paste(collapse = ",")

  # get html path    
  html_path <-
    Rmd_file %>%
    get_html_path_from_Rmd()

  if (is.null(html_path)) {
    abort(glue("Please knit {basename(Rmd_file)} !"))
  }
  

  post_param <- rlang::exec(
    post_interactively,
    title = title,
    tag = tag,
    html_path = html_path
  )

  result <- post_html(post_param$title, post_param$tag, post_param$html_path)
  print(result)
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


#' APIへPOSTを行う
#'
#' エンコードもここでする
#' @param title character タイトル
#' @param tag character タグ
#' @param html_path character htmlファイルへのパス
#'
#' @return character POSTした結果
#'
#' @export
#' @examples
#' post_html("テストPOST", "test, Post,テスト ", "tools/test_post_to_api.html")
post_html <- function(title, tag, html_path){
  result <-
    httr::POST(url = Sys.getenv("TARGET_API"),
               body = httr::upload_file(path = html_path, type = "text/html"),
               httr::add_headers(title = title %>% encoding_title(),
                                 tags = tag %>% encoding_tag()),
               encode = "raw")

  return(result)
}


#' POST時のshiny起動
#'
#' headerに
#'
post_interactively <- function(title, tag, html_path) {

  # This will be assigned in Shiny's server function

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
                            html_path = html_path)
        shiny::stopApp(returnValue = post_param)
    })

  }

  viewer <- shiny::dialogViewer("Preview", width = 1200, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)

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
  #path_input <- shiny::textInput(inputId = "path", label = "Path", value = html_path)

  # Preview
  html_text_for_preview <- readr::read_file(html_path)
  preview_html <- shiny::HTML(html_text_for_preview)

  miniUI::miniPage(
    title_bar,
    miniUI::miniContentPanel(
      shiny::fluidRow(shiny::column(title_input, width = 6), shiny::column(parent_input, width = 6)),
      #shiny::fluidRow(shiny::column(path_input, width = 12)),
      shiny::fluidRow(shiny::column(tag_input, width = 12)),
      shiny::hr(),
      shiny::div(preview_html)
    )
  )
}
