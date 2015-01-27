draw.pca <-
function(display.in.diagram = c('sites'),
                     display.species = c('none'),
                     display.sites = c('points'),
                     axes.shown = c(1,2),
                     display.EIV = FALSE,
                     display.header = FALSE,
                     display.envelope = FALSE,
                     header.name = 'env',
                     display.header.style = c('arrow'),
                     display.spider,
                     display.group.center = FALSE,
                     three.dim = FALSE,
                     resolution = c(1280, 768),
                     bw = FALSE,
                     ...)

{
  newRversion <- check.install (display.spider)
  open.r.window(three.dim, resolution)
  if (newRversion) pb <- myTkProgressBar (paste (ifelse (three.dim, '3D', '2D'),'PCA - Analysis progress'), 'Importing data from JUICE', 0, 100, 20) else pb <- NULL

  write ('End of ordination', file='result.txt')
  library (vegan)
  input.data <- read.check.data (display.sites = display.sites, display.EIV = display.EIV, display.header = display.header, display.envelope = display.envelope, display.spider = display.spider, display.group.center = display.group.center)

  # 2. update progress bar
  if (newRversion) setTkProgressBar (pb, label = 'Calculation of ordination', value = 40)

## calculation of ordination
  last.result <-  use.last (input.data, 'pca', setting = list (nic = 0))
  if (last.result$use.last.result) spec.data.ord <- last.result$last.data.result  else
  spec.data.ord <- prcomp(input.data$spec.data, scale = TRUE)

  # 3. update progress  bar
  if (newRversion) setTkProgressBar (pb, label = 'Saving results', value = 60)

  save.ord.result (spec.data.ord, last.result$use.last.result, 'pca', input.data$deleted.plots)  

# 3. update progress  bar
  if (newRversion) setTkProgressBar (pb, label = 'Drawing the figure', value = 80)
  
  if (three.dim)  
    draw.3d(input.data = input.data, spec.data.ord = spec.data.ord, display.in.diagram = display.in.diagram, display.species = display.species, display.sites = display.sites, axes.shown = axes.shown, display.EIV = display.EIV, display.header = display.header, display.envelope = display.envelope, header.name = header.name, display.header.style = display.header.style, display.spider = display.spider, display.group.center = display.group.center, pb = pb) else
    draw.2d(input.data = input.data, spec.data.ord = spec.data.ord, display.in.diagram = display.in.diagram, display.species = display.species, display.sites = display.sites, axes.shown = axes.shown, display.EIV = display.EIV, display.header = display.header, display.envelope = display.envelope, header.name = header.name, display.header.style = display.header.style, display.spider = display.spider, display.group.center = display.group.center, bw = bw, pb = pb)
  
  if (!last.result$use.last.result)
    {
    last.data <- list (last.matrix.sum = sum(input.data$spec.data), last.matrix.species = colnames (input.data$spec.data), last.matrix.sites = rownames (input.data$spec.data), last.result = spec.data.ord)
    save (last.data, file = 'pca_lfa.r')
    last.data.quick <- list (type.of.analysis = 'pca', size.of.matrix = dim (input.data$spec.data), setting = list (nic = 0))
    save (last.data.quick, file = 'pca_lfq.r')
    }
}

