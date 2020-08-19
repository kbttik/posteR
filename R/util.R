#' Example package to say hello
#' @importFrom rlang abort
#' @importFrom rlang exec
#' @importFrom glue glue
#' @importFrom stringr str_replace_all
#' @importFrom magrittr %>%
"_PACKAGE"

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


#' rstudioのプロンプトを作成
#'
#' 設定に必要な値を入力させる
#'
#' @param title character プロンプト上には表示されない
#' @param message character プロンプト上に表示される
#' @param default character 入力値のデフォルト
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


#' gitのレポジトリ名の取得
#'
#' "/"," "で区切り、.gitの部分を抽出する
#'
#' @param path character パスのgitレポジトリ名を取得する
#' @return character レポジトリ名
#' @export
#' @examples
#' get_git_repository()
get_git_repository <-function(path = "."){
  repo_name <-
    system("git remote -v", intern = TRUE)[1] %>%
    stringr::str_split("(/| )", simplify = TRUE) %>%
    stringr::str_subset("\\.git") %>%
    stringr::str_replace("\\.git", "")

  return(repo_name)
}

#' htmlまたはnotebookにknitしているか確認
#' 
#' Rmdファイルのタイトルの.html .nb.htmlが存在しているかチェック
#' @param Rmd_file character Rmdへのパス
#' @return character htmlへのパス、なければNULL
get_html_path_from_Rmd <- function(Rmd_file){
  path_html <- Rmd_file %>% stringr::str_replace("\\.Rmd", "\\.html")
  path_notebook <- Rmd_file %>% stringr::str_replace("\\.Rmd", "\\.nb\\.html")
  
  if (file.exists(path_html))
    return(path_html)
    
  if (file.exists(path_notebook))
    return(path_notebook)
  
  return(NULL)
}
