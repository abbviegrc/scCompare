library(jsTreeR)
library(shiny)

# small function to shorten the Shiny values
Text <- function(value) {
  lapply(value, `[[`, "text")
}
css <- "
i.jstree-themeicon-custom {
  background-size: contain !important;
}
"

yaml <- "
cell:
    epithelial cell:
      squamous epithelial cell:
          Keratinocyte:
    Melanocyte:
    Sectretory cell:
    hematopoietic cell:
      leukocyte:
        myeloid leukocyte:
          macrophage:
          mast cell:
          dendritic cell:
            Langerhans cell
            conventional dendritic cell
          monocyte:
            classical monocyte:
            non classical monocyte:
          lymphocyte:
            B cell:
            mature B cell:
              memory  B cell:
            precursor B cell:
            T cell:
                effector T cell:
                gamma delta T cell:
                mature NK T cell:
                memory  T cell:
                mucosal variant T cell:
                naïve T cell:
                regulatory T cell:
                CD4 positive alpha beta T cell:
                   CD4 positive alpha beta cytotoxic T cell:
                   T follicular helper cell:
                   T helper 1 cell:
                   T helper 17 cell:
                   central memory CD4 positive alpha beta T cell:
                   effector memory CD4 positive alpha beta T cell:
                   naïve thymus derived CD4 positive alpha beta T cell:
                CD8 positive alpha beta T cell:
                    CD8 positive alpha beta cytotoxic T cell:
                    central memory CD8 positive alpha beta T cell:
                    effector memory CD8 positive alpha beta T cell:
                    naïve thymus derived CD8 positive alpha beta T cell:
            innate lymphoid cell:
                group 1 innate lymphoid cell:
                    natural killer cell:
                group 3 innate lymphoid cell:
            hematopoietic precursor cell:
                hematopoietic lineage restricted progenitor cell:
                  pre conventional dendritic cell:
                  pro B cell:
"

# library(yaml)
# osList <- yaml.load(yaml)
# osNode <- data.tree::as.Node(osList,interpretNullAsList=TRUE)
# nodes <- list(
#   list(
#     text = "epithelial cell",
#     children = list(
#       list(
#         text = "squamous epithelial cell",
#         children = list(
#           list(
#             text = "Keratinocyte"
#           )
#         )
#       )
#     )
#   ),
#   list(
#     text = "Keratinocyte"
#   ),
#   list(
#     text = "Sectretory cell"
#   ),
#   list(
#     text = "hematopoietic cell",
#     children = list(
#       list(text = "leukocyte"),
# 
#       children = list(
#         list(text = "myeloid leukocyte",
# 
# 
#              children = list(
#                list(text = "macrophage"),
#                list(text = "mast cell"),
#                list(text = "dendritic cell")
#         )
# 
# 
#       )
#     )
#   )
# )
# )
# nodes <- list(
#   list(
#     text = "epithelial cell",
#     children = list(
#       list(
#         text = "squamous epithelial cell"
#       ),
#       list(
#         text = "Keratinocyte"
#       )
#     )
#   ),
#   list(
#     text = "Melanocyte"
#     
#   ),
#   list(
#     text="Sectretory cell"
#     
#   ),
#   list(
#     "hematopoietic cell",
#     children = list(
#       list(
#         text = "leukocyte",
#         list(
#           text = "myeloid leukocyte"
#         ),
#         list(
#           text = "macrophage"
#         ),
#         list(
#           text = "mast cell"
#         ),
#         list(
#           text = "dendritic cell"
#         )
#       )
#     )
#   )
#   )
# 
# # nodes list for both trees
# Read the CSV file
data <- read.csv("./Bioturing_Cell_Ontology.csv",
                  stringsAsFactors = F)


# create a function to recursively build the tree
# read csv file

# create a function to recursively add children to a node
add_children <- function(node) {
  children <- data$child[data$parent == node$text]
  if (length(children) > 0) {
    # add children to current node
    node$children <- lapply(children, function(child) {
      # create node for child and add its children recursively
      child_node <- list(text = child,icon = "fa-solid fa-circle-nodes")
      add_children(child_node)
    })
  }
  node
}

# create a list for each parent node and add children recursively
root_nodes <- unique(data$parent)
parent_list <- lapply(root_nodes, function(root) {
  root_node <- list(text = root,
                    icon = "fa-solid fa-circle-nodes")
  add_children(root_node)
})

# combine parent lists into a single list
result <- parent_list[1]
nodes<-result

