

#' Converts a data.frame to Asciimint versions (latex, html, asciidoc pure)
#'
#' @param data.df
#' The data.frame to create table for. If it is list of data.frames of the same size then they would be mixed regarding the mixture.mode.
#' @param out.type
#' @param html.lib
#' @param latex.lib
#' @param dirname.s
#' @param filename.s
#' @param caption
#' @param the.style
#' @param file.out
#' @param file.here
#' @param ...
#' the lib specific parameters. If more than one lib is specified then the first one.
#'
#' @return
#' @export
#'
#' @examples
asciimint_table <- function(
  data.df,
  caption = '',
  the.style = c('asciimint'),
  out.type = c('all', 'latex', 'html','asciidoc'),
  html.lib = c('plotly'),
  latex.lib = c('xtable'),
  mixture.mode = c('sub','sub.and.super','super'),
  be.bare = FALSE,
  file.out = FALSE,
  file.here = FALSE,
  dirname.s,
  filename.s,
  ...
){

  # writeLines(str(style))

  # match arguments
  the.style <- match.arg(the.style)
  out.type <- match.arg(out.type)
  html.lib <- match.arg(html.lib)
  latex.lib <- match.arg(latex.lib)
  mixture.mode <- match.arg(mixture.mode)



  the.string <- ''
  # call the right convertor(s)
  if((out.type == 'all') || (out.type == 'latex')){
    the.string <- append(
      the.string,
      latex_table(
        data.df = data.df,
        caption = caption,
        the.style = the.style,
        be.bare = be.bare,
        ...
      )
    )

  }
  if((out.type == 'all') || (out.type == 'html')){
     the.string <- append(
      the.string,
      html_table(
        data.df = data.df,
        caption = caption,
        the.style = the.style,
        be.bare = be.bare,
        ...
      )
    )
  }
  if((out.type == 'all') || (out.type == 'asciidoc')){
     the.string <- append(
      the.string,
      asciidoc_table(
        data.df = data.df,
        caption = caption,
        the.style = the.style,
        be.bare = be.bare,
        ...
      )
    )
  }
  the.string <- paste(the.string, collapse = '\n\n')



  # do the right output thing for 'string' or 'file' or 'both'
  if(file.out){

    ## working directory is default, if not provided
    current.wd <- getwd()
    if (missing(dirname.s)){
      dirname.s <- current.wd
    }

    ## for filename the dataframe variable name is default, if not provided
    if (missing(filename.s)){
      filename.s <- paste(deparse(substitute(data.df)), '.adoc.txt', sep = '')
    }

    #write to destination
    out.path <- file.path(dirname.s,filename.s)
    write(the.string, file = out.path)

    # make a copy to the current dir if file.here is TRUE and you have not wrote it already
    if (file.here == TRUE && dirname.s != current.wd){
      here.path <- file.path(current.wd, filename.s)
      write(the.string, file = here.path)
    }

  }


  return(
    list(
      data.df = data.df,
      the.string = the.string
    )
  )


}

#' Title
#'
#' @param data.df
#' @param ...
#' @param the.style
#' @param lib
#' @param caption
#'
#' @return
#' @export
#'
#' @examples
latex_table <- function(
  data.df,
  caption = '',
  the.style = c('asciimint'),
  lib=c('xtable'),
  be.bare = FALSE,
  ...){

  the.style <- match.arg(the.style)
  lib <- match.arg(lib)


  the.prefix <- paste(
    '[.latextable]',
    '++++',
    '<remark role="latexcode">',
    '<![CDATA[',
    sep = "\n")

  the.postfix <- paste(
    ']]>',
    '</remark>',
    '++++',
    sep = "\n"
  )


  if (lib == 'xtable'){

    require('xtable')

    # setup styles
    if (the.style == 'asciimint'){
      style.set <- list(
        table.placement = '',
        NA.string = '',
        print.results = FALSE,
        booktabs = TRUE,
        include.rownames = FALSE
      )
    }


    g <<- c(
        list(x = xtable::xtable(x = data.df, caption = caption)),
        c(
          type = 'latex',
          style.set
        ),
        ...)
    #create the main output (with stylee)
    latex.string <- do.call(
      xtable::print.xtable,
      c(
        list(x = xtable::xtable(x = data.df, caption = caption)),
        c(
          type = 'latex',
          style.set
        ),
        ...
      )
    )


    # add surrondings if not be.bare
    if (be.bare){
      the.result <- latex.string
    }else{
      the.result <- paste(the.prefix, latex.string, the.postfix, sep = '')
    }

    return(the.result)

  }



}


#' Title
#'
#' @param data.df
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
html_table <- function(
  data.df, caption = '',
  the.style = c('asciimint'),
  lib=c('xtable'),
  be.bare = FALSE,
  ...
){


  the.style <- match.arg(the.style)
  lib <- match.arg(lib)

  the.prefix <- paste(
    '[.htmltable]',
    '++++',
    '<remark role="htmlcode">',
    '<![CDATA[',
    sep = "\n")

  the.postfix <- paste(
    ']]>',
    '</remark>',
    '++++',
    sep = "\n"
  )


  if (lib == 'xtable'){

    require('xtable')

    # setup styles
    if (the.style == 'asciimint'){
      style.set <- list(
        table.placement = '',
        NA.string = '',
        print.results = FALSE,
        booktabs = TRUE,
        include.rownames = FALSE
      )
    }


    #create the main output (with stylee)
    latex.string <- do.call(
      xtable::print.xtable,
      c(
        list(x = xtable::xtable(x = data.df, caption = caption)),
        c(type = "html",
        style.set),
        ...
      )
    )

    the.result <- paste(the.prefix, latex.string, the.postfix, sep = '')

    return(the.result)

  }

}


#' Title
#'
#' @param data.df
#' @param ...
#' @param lib
#' @param the.style
#'
#' @return
#' @export
#'
#' @examples
asciidoc_table <- function(
  data.df,
  caption = '',
  the.style = c('asciimint'),
  lib=c('ascii'),
  be.bare = FALSE,
  ...
){


  the.style <- match.arg(the.style)
  lib <- match.arg(lib)

  the.prefix <- paste(
    '[.amtable]',
    '|===',
    sep = "\n")

  the.postfix <- paste(
    '|===',
    sep = "\n"
  )


  if (lib == 'ascii'){

    require('ascii')

    # setup styles ? ascii.table
    if (the.style == 'asciimint'){
      style.set <- list(
        align = '',
        na.print = '',
        header = TRUE,
        include.colnames = TRUE,
        include.rownames = FALSE
      )
    }


    #create the main output (with stylee)
    the.asciidoc.table <- do.call(
      ascii::ascii,
      c(
        list(x = data.df),
        c(style.set),
        ...
      )
    )
    asciidoc.string <- do.call(
      ascii::print,
      c(
        list(x = the.asciidoc.table),
        type = 'asciidoc'
      )
    )

    the.result <- paste(the.prefix, asciidoc.string, the.postfix, sep = '')

    return(the.result)

  }

}

