
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "lineDensity",
  description = "Calculates a magnitude-per-unit area (density in km/km2) from polyline features that fall within a specified radius around each cell of the output `RasterLayer`", #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = c(person(c("Mario", "Dennis"), "van Telgen", email = "mario.vantelgen@outlook.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.4", lineDensity = "0.3.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "lineDensity.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("features", "character", NA, NA, NA, "This describes the features to include"),
    defineParameter("searchRadius", "numeric", 100, 0, Inf, "This describes the search radius (meters) within which to calculate density. Minimum value is resolution/2 (auto check and set in effect)"),
    defineParameter("resolution", "numeric", 250, 0, Inf, "This describes the cell hight and width in meters"),
    defineParameter("templateLayer", "character", NA, NA, NA, "Character (pointing to existing object) or Spatial* or Raster* object which is used as template for the extent, dimensions and resolution of the output raster"),
    defineParameter("method", "character", default = c("FFT", "focal", "python"), NA, NA, "This describes the method used to calculate density (Currently: 'FFT', 'focal', 'python'"),
    defineParameter("boundaryEffect", "character", "lethal", NA, NA, "This describes boundary effects for dispersal (currently available: ‘solid’ (or ‘reflecting’), 'lethal' (or ‘absorbing’)"),
    defineParameter("noiseRemoval", "logical", TRUE, NA, NA, "Should noise be corrected (negative values set to '0') in final raster"),
    defineParameter("splitRaster", "logical", TRUE, NA, NA, "Should convolution take place over a raster that is split in smaller tiles to increase speed of fft?"),
    defineParameter("tileDims", "numeric", c(512, 512), 0, Inf, "Maximum dimensions of tiles created by splitRaster"),
    defineParameter("startTime", "numeric", 0, NA, NA, "Simulation time at which to initiate aging"),
    defineParameter("increment", "numeric", 1.0, NA, NA, "Time interval between density calculation events"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".writeInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first write event should occur"),
    defineParameter(".writeInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between write events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    # expectsInput(objectName = "seismicLines", objectClass = "SpatialLinesDataFrame", desc = "Seismic line layer"),
    expectsInput(objectName="studyArea", objectClass="SpatialPolygonsDataFrame", desc="SPDF of study area"),
    expectsInput(objectName="rasterToMatch", objectClass="RasterLayer*", desc="raster to which the output raster layer will be matched. Often raster of studyArea"),
    expectsInput(objectName="waterRaster", objectClass="RasterLayer*", desc="raster used for masking. Sets all raster cells that are classified as water to 'NA'"),
    expectsInput(objectName="anthrDisturb", objectClass = "SpatialLinesDataFrame", desc = "Anthropogenic disturbance vector data layer")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "LFDensMap", objectClass = "RasterLayer", desc = "Linear feature density map")
  )
))

## event types
doEvent.lineDensity = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      tictoc::tic("Init time:")
      sim <- Init(sim)
      tictoc::toc()
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$startTime, "lineDensity", "density")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "lineDensity", "plot")
      sim <- scheduleEvent(sim, P(sim)$.writeInitialTime, "lineDensity", "write")
    },
    plot = {
      # do stuff for this event
      plotFun(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "lineDensity", "plot")
    },
    density = {
      # do stuff for this event
      tictoc::tic("densFun time:")
      sim <- densFun(sim)
      tictoc::toc()
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$increment, "lineDensity", "density")
    },
    write = {
      # do stuff for this event
      raster::writeRaster(sim$LFDensMap, 
                          filename = paste("lineDensityMap",
                                           "_v", moduleVersion(currentModule(sim)), 
                                           "_m", P(sim)$method, "_r", P(sim)$searchRadius, 
                                           "_t", time(sim)[1], 
                                           "_", paste(unlist(sim$inputObjLineDensity), collapse = "_"),
                                           ".tif", sep=""), 
                          format="GTiff", overwrite = TRUE)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.writeInterval, "lineDensity", "write")
    },
    save = {
      # do stuff for this event
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "lineDensity", "save")
    },
    summary = {
      # do stuff for this event
      # sim <- summaryFun(sim)
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + increment, "lineDensity", "summary")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

### initialization
Init <- function(sim) {
  # Minimum searchRadius is resolution/2. Check and set.
  if(P(sim)$searchRadius < P(sim)$resolution/2) { 
    params(sim)$lineDensity$searchRadius <- P(sim)$resolution/2 
  }
  
  # select features from anthrDisturb layer
  if(suppliedElsewhere("anthrDisturb", sim)){
    if(P(sim)$features[1] == "all" | is.na(P(sim)$features[1])){ # select if layers specified
      message("All features in 'sim$anthrDisturb' selected.")
    } else {
      idx <- P(sim)$features %in% sim$anthrDisturb$Class
      message(paste("c(", paste(P(sim)$features[which(idx)], collapse = ", "), ") in 'sim$anthrDisturb' selected.", sep=""))
      idx <- sim$anthrDisturb$Class %in% P(sim)$features
      if(any(idx)){
        sim$anthrDisturb <- sim$anthrDisturb[idx,]
        idx <- !(P(sim)$features %in% sim$anthrDisturb$Class)
        if(any(idx)){
          message(paste("Feature class '", P(sim)$features[which(idx)], "' ignored. Does not occur in 'sim$anthrDisturb'", sep=""))
        }
      } else {
        stop(paste("Need to specify an existing feature class of 'anthrDisturb'"))
      }
    }
  }
  
  # select all objects that are provided as input objects and marked as inputObjects for this module.
  # inputObj <- as.list(inputObjects(sim, currentModule(sim))[[1]][which(inputObjects(sim, currentModule(sim))[[1]] %in% objects(sim))])
  sim$inputObjLineDensity <- list("anthrDisturb") # used for writing
  
  # get and list these input objects
  inputObjList <- lapply(sim$inputObjLineDensity, get, envir=sim)
  
  # Check and set Class IDs
  checkNames <- lapply(inputObjList, "names")
  idx <- unlist(lapply(checkNames, function(x){ !("Class" %in% x)}))
  if(any(idx)){
    idx <- which(idx)
    message(paste("No feature classes specified in lines object '", sim$inputObjLineDensity[[idx]],
                  "'. Column added called 'Class' classifying features as '", sim$inputObjLineDensity[[idx]] ,
                  "', which will be used for feature selection.", sep=""))
    inputObjList[[idx]]@data$Class <- sim$inputObjLineDensity[[idx]]
    params(sim)$lineDensity$features <- c(params(sim)$lineDensity$features, sim$inputObjLineDensity[[idx]])
  }
  
  # Prepare lines for density calculations
  sim$linesForDensity <- prepLines(inputObjList, template=sim$studyArea) # also works for single object
  sim$linesForDensityOld <- sim$linesForDensity # keep track of line renewal
  
  # Set map extent and resolution from rasterToMatch, studyArea or by extending the input layer
  # so that extent is a multiple of the map resolution (i.e. resolution remains the same in psp and pixellate)
  
  if(!suppliedElsewhere('templateExt', sim)){
    if(suppliedElsewhere("rasterToMatch", sim)){ # assume it points to studyArea
      message("rasterToMatch supplied. Extent and resolution used.")
      sim$templateExt <- raster::extent(sim$rasterToMatch) #assume studyArea extent
      sim$res <- raster::res(sim$rasterToMatch)
    } else
      if(suppliedElsewhere("studyArea", sim)){
        message("No rasterToMatch supplied. Assume extent of studyArea.")
        sim$templateExt <- raster::extent(sim$studyArea)
      } else {
        message("No rasterToMatch or studyArea supplied. Assume manual settings for resolution and extent if available, else use default resolution and extent of (merged) input object")
        sim$templateExt <- raster::extent(sim$linesForDensity) # set templateExt to input spatialLines object extent    }
      }
  }
  
  # set global resolution  
  if(!suppliedElsewhere('res', sim)){ # in case studyArea is not provided
    if(suppliedElsewhere("mapRes", sim)) { #or from other source
      if(length(sim$mapRes) == 1){
        sim$res <- rep(sim$mapRes,2)
      } else {
        sim$res <- sim$mapRes
      }
    } else if(!is.na(P(sim)$resolution)) { #Else use resolution from module parameters
      if(length(P(sim)$resolution) == 1){
        sim$res <- rep(P(sim)$resolution,2)
      } else {
        sim$res <- P(sim)$resolution
      }
    } else { #Stop if not
      stop("Need to provide map resolution")
    }
  }
  
  # browser("check bboxes")
  # Adjust linesForDensity extent to template extent
  sim$linesForDensity@bbox <- sp::bbox(sim$templateExt)
  
  # Initialize convolution space
  #
  message("Creating convolution space")
  cSpace <- raster::raster(sim$linesForDensity, resolution=sim$res)
  
  #     To allow the densities to bleed into the edges during FFT we need to buffer the extent 
  #     of the convolution space with searchRadius on all sides.
  bleedBuffer <- P(sim)$searchRadius/sim$res # number of cells to buffer each tile with [rows, cols]
  
  #     To save time and memory perform all calculations on a as small as possible landscape.
  #     The landscape will be matched to rasterToMatch after calculations.
  #     
  #     Discrete Fourier Transform (DFT) algorithms are most efficient for “Highly Composite Numbers", 
  #     specifically multiples of (2,3,5). On binary systems, multiples of 2 are most efficient(?)
  #     Therefore the dimensions of the convolution space are set to match the first next high 
  #     composite number; i.e. multiple of 2 (2^n)
  #     Fft creates noise and on large landscapes fft creates ghosting (very small (within the 1e-15 - 1e-17 
  #     range) noise values that are visible as shadow of the real density estimates.
  #     If available anti-aliassing software may be able to reduce this noise, but running fft-calculations
  #     on smaller landscapes (subset of a larger landscape may be better practice given that not all systems
  #     have anti-aliasing capability and this software does not deal with all the noise created during fft.
  #     NOTE: Ghosting does seems to be reduced when fft is calculated in high composite number space. 
  #     (Needs real tests?) and small landscapes.
  # 
  #     Spending time on additional operations to create and run FFT on one larger landscape or multiple smaller
  #     landscapes, only pays off when the speed gained by optimizing to high composite dimension lengths 
  #     outweighs the time needed for these additional operations.
  #     Furthermore, optimizing the raster extents only works when the object is very large, 
  #     i.e. (for now let's say if length of raster is greater than 2**20)
  #     For more detail see: https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=4&ved=2ahUKEwjppvCtjonhAhVQIqwKHZoNBQkQFjADegQIBxAC&url=https%3A%2F%2Fcran.r-project.org%2Fweb%2Fpackages%2Fpsd%2Fvignettes%2Ffftw.pdf&usg=AOvVaw0LCjNRmZZtB6UDTBW9Lzio
  if(P(sim)$method == "FFT" & length(cSpace) > 2**20){ # fft on Non-high composite numbers get very slow on numbers 
    # optimize space for fft
    message("Speed optimization in effect. Dimensions set to high composite numbers (multiples of 2)")
    
    if(P(sim)$splitRaster){ # split cSpace in 
      message("Convolution space is being split in smaller tiles")
      # Create split raster
      #     Each tile should have dimensions that are high composite number. That means that each tile should 
      #     have a number of rows and columns minus 2x the edgeBuffer.
      #
      # Set minimum tile dimensions
      #     set tile dimensions to next composite of 2 number, larger than 
      minTileDims <- 2^ceiling(log2(2*bleedBuffer)) - (2*bleedBuffer) # minimal required tile dimensions
      # To reduce cpu and memory usage set a maximum tile size
      tileDims <- P(sim)$tileDims
      if(any(minTileDims > tileDims)) {
        message(paste("tileDims to small, minTileDims of c(", paste(minTileDims, collapse = ", "), ") used", sep=""))
        tileDims <- minTileDims
          
      }
      
      corrTileDims <- tileDims-(2*bleedBuffer)
      
      # Get dimensions of cSpace
      cSpaceDims <- dim(cSpace)[1:2]
      
      # Determine the number of tiles
      nTiles <- ceiling(cSpaceDims/corrTileDims)
      targetDims <- nTiles*corrTileDims
      addCells <- (targetDims-cSpaceDims)/2
      # extend cSpaceSplit so its extent is a multiple of minTileDims
      cSpace <- raster::extend(cSpace, addCells)
      #     set extent
      fullExt <- raster::extent(cSpace[[1]])
      
      # Create split cSpace
      cSpace <- splitRaster(cSpace, nx=nTiles[2], ny=nTiles[1], buffer=bleedBuffer, path=tempdir())
      
      # create vector with expand plan to expand edge tiles to 
      needExpand <- !lapply(cSpace, function(x){dim(x)[1:2]}) %in% list(tileDims)
      
      if(any(needExpand)){ # extend
        tmp <- matrix(nrow=nTiles[1], ncol=nTiles[2])
        rownames(tmp) <- 1:NROW(tmp)
        colnames(tmp) <- 1:NCOL(tmp)
        
        # set
        tmp[1,] <- sapply(tmp[1,], FUN=function(x){ 
          paste(x[!is.na(x)], "bottom", sep="")})
        tmp[NROW(tmp),] <- sapply(tmp[NROW(tmp),], FUN=function(x){ 
          paste(x[!is.na(x)], "top", sep="")})
        tmp[,1] <- sapply(tmp[,1], FUN=function(x){ 
          paste(x[!is.na(x)], "left", sep="")})
        tmp[,NCOL(tmp)] <- sapply(tmp[,NCOL(tmp)], FUN=function(x){ 
          paste(x[!is.na(x)], "right", sep="")})
        
        # make vector to go with splitRasters
        expandPlan <- c(tmp)
      }

      # store objects for later use
      sim$splitRasterPars <- list(
        needExpand = needExpand,
        expandPlan = expandPlan,
        nTiles = nTiles,
        bleedBuffer = bleedBuffer,
        tileDims = tileDims,
        addCells = addCells
      )
            
    } else {
      cSpace <- raster::extend(cSpace, (2*bleedBuffer))
      
      #     1) compute new optimal landscape dimensions from the binary logarithm of the searchRadius-buffered
      #     landscape dimensions rounded up to the next larger even integer.
      cSpaceDims <- dim(cSpace)[1:2]
      optimalDims <- 2^(ceiling(log2(cSpaceDims))) # next larger integer
      
      #     Optional: Save time? and take the smallest of 2,3,5 composites
      # optimalDims <- rbind('2' = 2^(ceiling(log2(cSpaceDim))), # next larger integer
      #                      '3' = 3^(ceiling(log(cSpaceDim, base=3))), # next larger integer
      #                      '5' = 5^(ceiling(log(cSpaceDim, base=5)))) # next larger integer
      # optimalDims <- c(min(optimalDims[,1]),min(optimalDims[,2]))
      
      #     2) calculate the number of rows and columns that need to be added (on each side of the raster)
      addCells <- (optimalDims-cSpaceDims)/2 # rows and columns to be added on each side
      #     3) recreate cSpace with optimal extent
      cSpace <- list(raster::extend(cSpace, addCells)) # optimal cSpace
      #     set extent
      fullExt <- raster::extent(cSpace[[1]])
      
    }
  }
  # Set observation window from larger extent
  # sim$W <- spatstat::as.owin(spatial.tools::bbox_to_SpatialPolygons(sp::bbox(fullExt)),
                             # proj4string=sp::proj4string(sim$linesForDensity))
  
  rm(inputObjList); gc() # clear memory. Remove unused objects.
  
  # Set project projection
  if(!is.na(raster::crs(sim$linesForDensity))){
    proj.crs <- raster::crs(sim$linesForDensity)
  } else {
    proj.crs <- NA
  }
  
  # Methods setup
  switch(P(sim)$method,
         focal = {
           # Prep focal approach
           # focalWeights()
         },
         FFT = {
           
           # set up FFT landscape
           message("creating convolution space")
           # Identify map dimensions and resolution
           # convolution space parameters
           idx <- which(!needExpand)[1] # use first tile that does not require expansion as template for kernel development
           ps <- raster::res(cSpace[[idx]]) # resolution # [[1]] works on lists and vectors
           # fullExt <- extent(cSpace[[1]]) # extent
           # fullExt <- sim$templateExt
           fullExt <- fullExt
           mapDim <- dim(cSpace[[idx]]) # map dimensions # [[1]] works on lists and vectors
           npx <- mapDim[2] # number of cells along x
           npy <- mapDim[1] # number of cells along y
           
           # Create kernel space
           xl <- ps[1]*npx # maximum and minimum x coordinate
           yl <- ps[2]*npy # maximum and minimum y coordinate
           
           
           # method using focalWeight() to create K
           # create focal weight
           weight <- raster::focalWeight(cSpace[[1]], d = P(sim)$searchRadius, type = 'circle')
           # store max weight for use in noise correction
           sim$maxWeight <- max(weight)
           
           # expand weight to target
           # if(P(sim)$boundaryEffect == "solid"){
             # target <- matrix(nrow=npy, ncol=npx)
           # } else 
           if(P(sim)$boundaryEffect == "lethal"){
             target <- matrix(nrow=2*npy, ncol=2*npx)
           }
           
           # Calculate the convolution (fft) of the dispersal kernel; 
           message("Calculating kernel convolution")
           # sim$fK <- stats::fft(expandMatrix(weight, target))
           sim$fK <- fftw::FFT(expandMatrix(m=weight, target=target, sides="all"))
           
           # store parameters of convolution space
           sim$cSpacePars <- list( # store cSpace parameters for later use
             crs = proj.crs,
             fullExt = fullExt,
             dim = mapDim, # map dimensions
             res = ps, # resolution (pixel size) (m)
             npx = npx, # number of cells along x             #required later
             npy = npy # number of cells along y             #required later
             # xl = xl, # maximum and minimum x coordinate
             # yl = yl, # maximum and minimum y coordinate
             # dx = dx, # spatial step delta_x = 2*xl/npx
             # dy = dy, # spatial step delta_y = 2*yl/npy
             # dt = dt # time step delta_t
             
           )
           
           
           # sim$wSpacePars <- list( # store cSpace parameters for later use
           #   crs = proj.crs,
           #   ext = extW,
           #   dim = mapDimW, # map dimensions
           #   res = psW, # resolution (pixel size) (m)
           #   npx = npxW, # number of cells along x             #required later
           #   npy = npyW # number of cells along y             #required later
           #   # xl = xl, # maximum and minimum x coordinate
           #   # yl = yl, # maximum and minimum y coordinate
           #   # dx = dx, # spatial step delta_x = 2*xl/npx
           #   # dy = dy, # spatial step delta_y = 2*yl/npy
           #   # dt = dt # time step delta_t
           # )
           # browser() # MEMORY ISSUEs?:  reduce memory usage by overwriting unnecessary objects (set searchRadius to 1, and resolution to .025!!)
           
         })
  
  
  # ! ----- STOP EDITING ----- ! #
  
  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  Plot(sim$LFDensMap)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
densFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  
  gc()
  
  # assuming that all input objects remain 'SpatialLines' throughout the simulation the following will work.
  # If not, add check and conversion to 'SpatialLines' here.
  
  # select all objects that are provided as input objects and marked as inputObjects for this module.
  # inputObj <- as.list(inputObjects(sim, currentModule(sim))[[1]][which(inputObjects(sim, currentModule(sim))[[1]] %in% objects(sim))])
  # inputObj <- c("Roads")
  # get and list updated input objects
  inputObjList <- lapply(sim$inputObjLineDensity, get, envir=sim)
  
  # Coerce all object classes to SpatialLinesDataFrame
  # objClass <- lapply(inputObjList, class)
  # targetClass <- "SpatialLinesDataFrame" # SLDF cannot be binded to SpatialLines. All data needed here? or can drop?
  # idx <- which(!objClass %in% targetClass)
  # if(length(idx) > 0){ # evaluate/set class
  #   inputObjList <- lapply(inputObjList[idx], FUN= function(x) {SpatialLinesDataFrame(x, data=data.frame(1:length(x)))} )
  # }
  
  # bind all spatialLines objects that are provided in 'inputObjects' 
  # sim$linesForDensity <- do.call(rbind, inputObjList)
  
  sim$linesForDensity <- prepLines(inputObjList, template=sim$studyArea) # also works for single object
  # sim$linesForDensity <- do.call(prepLines, inputObjList, template=sim$studyArea) # also works for single object
  
  
  # Set extend to studyarea
  sim$linesForDensity@bbox <- sp::bbox(sim$cSpacePars$fullExt)
  # sim$linesForDensity@bbox <- sp::bbox(sim$templateExt)
  if( (time(sim) == P(sim)$startTime) | length(sim$linesForDensity@data) != length(sim$linesForDensityOld@data)){ # perform only at first run and when lines change 
    # !!!!!!  This works because we assume that lines are added only. Needs other comparisson to work when lines layer
    #         adds and removes lines to avoid skipping calculation when the same number of lines are added and removed 
    #         simultaneously !!!!!
    rm(inputObjList); gc() # clear memory. Remove unused objects.
    
    # ADD: run function on the condition that the linear features map has changed
    # sim$linesForDensityOld <- sim$linesForDensity # to keep track of spatial lines object at previous time unit. (move to doEvent??)
    # if(sim$linesForDensity == sim$linesForDensityOld){}
    
    # Converts a line segment pattern to a pixel image with width and height of pixels (res).
    message("Creating psp")
    psp <- Cache(maptools::as.psp.SpatialLinesDataFrame,
                 # window = sim$W,
                 from = sim$linesForDensity)
    
    message("Creating pxl")
    pxl <- Cache(spatstat::pixellate,
                 x=psp,
                 eps=sim$res[1])
    # pxl <- pixellate(as.psp.SpatialLinesDataFrame(sim$linesForDensity), eps=sim$res[1]) # Optional set dimyx (pixel array dimensions) if known.
    
    # pxl <- pixellate(as.psp(sim$linesForDensity, window=sim$W), eps=sim$res[1]) # Optional set dimyx (pixel array dimensions) if known.
    # pxl <- pixellate(as.psp.SpatialLinesDataFrame(sim$linesForDensity, W=sim$W), eps=sim$res[1]) # Optional set dimyx (pixel array dimensions) if known.
    # pxl <- pixellate(as.psp.SpatialLinesDataFrame(sim$linesForDensity, window=sim$W), eps=sim$res[1]) # Optional set dimyx (pixel array dimensions) if known.
    
    # Create raster (Check order of operation)
    message("Creating rstr")
    if(!is.na(raster::crs(sim$linesForDensity))) {
      rstr <- raster::raster(pxl, crs=crs(sim$linesForDensity))
    } else {
      rstr <- raster::raster(pxl)
    }
    
    # browser("store noiseLine for fft correction (sets all values after fft < noiseLine to '0')")
    sim$noiseLine <- min(rstr[rstr[]>0]) * sim$maxWeight
    
    # split rstr if required
    if(P(sim)$splitRaster & (P(sim)$method == "FFT")){
      # browser()
      message("length per cell raster is being split into smaller tiles")
      rstr <- splitRaster(rstr, nx=sim$splitRasterPars$nTiles[2], ny=sim$splitRasterPars$nTiles[1],
                          buffer=sim$splitRasterPars$bleedBuffer, path=tempdir())
    } else {
      rstr <- list(rstr)
    }
    # try rasterize/fasterize
    # browser()
    # rstr <- raster::rasterize(as(sim$linesForDensity,"SpatialLines"), resolution=sim$res[1], fun='length') # Optional set dimyx (pixel array dimensions) if known.
    
    rm(pxl); gc() # clear memory. Remove unused objects.
    
    # calculate line densities
    switch(P(sim)$method,
           FFT = {
             
             # set an expandplan to each tile
             for(i in 1: length(rstr)){
               attr(rstr[[i]],'expandPlan') <- sim$splitRasterPars$expandPlan[i]
             }
            # browser("check expandPlan with tiles")
             
             fftDisperse <- function(r, fK){
               # r = raster object
               # fk = convoluted dispersal kernel
               # ncol = number of columns (number of cells along the x axis)
               # nrow = number of rows (number of cells along the y axis)
               #
               
               # Expand rasterTiles that are on the edge of the lattice 
               
               sim$tile <- sim$tile+1
               
               # disperse per cell densities
               if(any(raster::getValues(r)>0)){ # Only perform dispersal if there are linelenghts to disperse
                 # Create array
                 # if(sim$tile == 12) browser("check dims r2")
                 
                 print(paste("Calculating tile no.", sim$tile, "/", prod(sim$splitRasterPars$nTiles), sep=""))
                 ext <- raster::extent(r)
                 crs <- sp::proj4string(r)
                 dims <- dim(r)[1:2]
                 ncol <- dims[2]
                 nrow <- dims[1]
                 # message("Creating rstr array")
                 r2 <- t(array(r[],c(ncol,nrow))) # rstr_T
                 
                 if(any(dims != sim$splitRasterPars$tileDims)){
                   r2 <- expandMatrix(r2, sim$splitRasterPars$tileDims, sides=attr(r,'expandPlan'))
                   dims2 <- dim(r2)[1:2]
                   ncol <- dims2[2]
                   nrow <- dims2[1]
                 }
                 
                 # Create mirrored image of lines layer
                 r2 <- rbind(r2, -r2[nrow:1,]) # r_T_y
                 r2 <- cbind(-r2[,ncol:1], r2) # r_T_x
                 # Computes the Discrete Fourier Transform using FFT
                 # message("Creating convolution of r2")
                 r2 <- fftw::FFT(r2) # r_T0
                 
                 # Spread
                 # message(paste("Calculating line densities. Method: ", P(sim)$method, sep=""))
                 if(P(sim)$boundaryEffect == "solid"){
                   browser()
                   r2 <- Re(fftshift(fftw::FFT(r2*fK, inverse=T)/length(fK))) # r_Td
                 } else if(P(sim)$boundaryEffect == "lethal") {
                   r2 <- Re((fftw::FFT(r2*fK, inverse=T)/length(fK))) # r_Td
                   
                   r2 <- matrix(r2, ncol = 2*ncol, nrow = 2*nrow)
                   r2 <- r2[seq((nrow+1),(2*nrow)),
                            -seq((ncol+1),(2*ncol))]; # Necessary reshape of matrix after lethal boundary dispersal
                 }
                 
                 # crop to initial dims remove rows and columns according to expandPlan
                 r2 <- cropMatrix(m=r2, target=r, sides=attr(r,'expandPlan'))
                 
                 # replace values in R with new values
                 r <- raster::raster(r2, template=r)
                 # if(sim$tile == 12) browser("check dims r2")
                 # dev();clearPlot();Plot(rtest);Plot(r, addTo="rtest");
                 # values(r) <- c(t(r2))
               }
               
               return(r)
             }
             
             sim$tile <- 0
             message(paste("Calculating line densities. Method: ", P(sim)$method, sep=""))
             message("Empty tiles will be skipped")
             rstr <- lapply(rstr, fftDisperse, fK=sim$fK)
             
             message("reconstructing raster from tiles")
             rstr <- mergeRaster(rstr, 
                                 fun = 'max') # overwrite lowest numbers (reduced densities due to edge effect)
             
             #Convert km2/km2 road density to km/km2
             rstr <- (rstr/prod(sim$res))*1e+03 # calculate m/m2 (devide by cellSize) and convert to km/km2 (*1000)
             
             # browser("Set noise removal")
             if(P(sim)$noiseRemoval){ # Sets all values after fft < noiseLine to '0'
               rstr[rstr[] < sim$noiseLine] <- 0 # remove noise (negatives) created by fft() # Check if correct
             }
             
             # rstr <- raster::raster(rstr, xmn=sim$cSpacePars$fullExt[1], xmx=sim$cSpacePars$fullExt[2],
             #                        ymn=sim$cSpacePars$fullExt[3], ymx=sim$cSpacePars$fullExt[4],
             #                        crs=sim$cSpacePars$crs)
             
             if(exists("templateExt", sim)){ # extend raster if templateLayer is provided
               if(sim$templateExt != sim$cSpacePars$fullExt){ # only need to extend or crop if different
                 if(sim$templateExt > sim$cSpacePars$fullExt){ #extend to templateExt
                   message("rstr extended to templateLayer extent")
                   rstr <- reproducible::Cache(raster::extend,
                                 x=rstr, y=sim$templateExt, value=0)
                 } else { #crop to templateExt
                   message("rstr cropped to templateLayer extent")
                   rstr <- reproducible::Cache(raster::crop,
                                 x=rstr, y=sim$templateExt)
                 }
               }
             }
             
           },
           focal = {
             # Run a circular radius density function (be mindful of edge effects)
             # Set up the moving window (equivalent to dispersal kernel)
             weight <- raster::focalWeight(rstr,d=P(sim)$searchRadius,type = "circle")
             
             # Calculate densities
             message(paste("Calculating line densities. Method: ", P(sim)$method, sep=""))
             rstr <- raster::focal(rstr, weight, fun=sum, filename = '', 
                                   na.rm=TRUE, pad=T, NAonly=FALSE, overwrite=TRUE)
             
             # Convert road density to km/km2
             rstr <- (rstr/prod(sim$res))*1e+03 # calculate m/m2 (devide by cellSize) and convert to km/km2 (*1000)
             
           })
    
    # Mask to remove water
    if(suppliedElsewhere('waterRaster', sim)){
      waterRasterExt <- raster::extent(sim$waterRaster)
      if(!suppliedElsewhere('waterRasterMask', sim)){
        if(sim$templateExt != waterRasterExt){
          if(sim$templateExt > waterRasterExt){ #extend to templateExt
            message("waterRaster extended to templateLayer extent")
            sim$waterRasterMask <- raster::extend(x=sim$waterRaster, y=sim$templateExt, value=0)
          } else {
            message("waterRaster cropped to templateLayer extent")
            sim$waterRasterMask <- raster::crop(x=sim$waterRaster, y=sim$templateExt)
          }
        }
      }
      message("Masking to 'waterRaster'")
      rstr[sim$waterRasterMask] <- NA
      # rstr <- reproducible::postProcess(x = rstr, rasterToMatch = sim$rasterToMatch,
      #                                  maskWithRTM = TRUE, destinationPath = tempdir(),
      #                                  filename2 = NULL)
    }
    
    # if(suppliedElsewhere('rasterToMatch', sim)){
    #   rstr <- reproducible::postProcess(x = rstr, rasterToMatch = sim$rasterToMatch,
    #                                    maskWithRTM = TRUE,
    #                                    destinationPath = tempdir(),
    #                                    filename2 = NULL)
    # }
    
    # Create density map
    sim$LFDensMap <- rstr
    
  } else {
    message("Lines have not changed. No need to calculate new line density layer")
  }
  
  sim$linesForDensityOld <- sim$linesForDensity # keep track of line renewal
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
summaryFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test
  
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  if(!suppliedElsewhere('studyArea', sim)){
    sim$studyArea <- reproducible::Cache(prepInputs, url = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU",
                                         targetFile = "BCR6_EcoregionsNWT.shp",
                                         alsoExtract = "similar")
    }
  
  # if(!suppliedElsewhere('Roads', sim)){
  #   RoadsUrl <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/RNF-FRR/files-fichiers/grnf061r10a_e.zip"
  #   sim$Roads <- Cache(prepInputs, url = RoadsUrl,
  #                      targetFile = "grnf061r10a_e.shp", # Be explicit and tractable
  #                      alsoExtract = "similar",
  #                      studyArea = sim$studyArea,
  #                      useSAcrs = TRUE,
  #                      overwrite = TRUE)
  # }
  # 
  if(!suppliedElsewhere('anthrDisturb', sim)){
    anthrDisturb.url <- "http://www.ec.gc.ca/data_donnees/STB-DGST/003/Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010.zip"
    sim$anthrDisturb <- Cache(
      prepInputs,
      url = anthrDisturb.url,
      targetFile = "EC_borealdisturbance_linear_2008_2010_FINAL_ALBERS.shp",
      alsoExtract = "similar",
      studyArea = sim$studyArea,
      useSAcrs = TRUE,
      overwrite = TRUE)
  }
  
  if(!suppliedElsewhere('rasterToMatch', sim)){
    sim$rasterToMatch <- cloudCache(prepInputs, url = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df", 
                              targetFile = "RTM.tif", destinationPath = tempdir(), 
                              studyArea = sim$studyArea,
                              useCloud = getOption("reproducible.useCloud", FALSE),
                              cloudFolderID = cloudFolderID, overwrite = TRUE, filename2 = NULL,
                              omitArgs = c("destinationPath", "cloudFolderID", "useCloud", "overwrite", "filename2")
                              )
  }
  
  if(!suppliedElsewhere('waterRaster', sim)){
    message("Creating masking layer 'waterRaster'")
    if(!suppliedElsewhere('wetLCC', sim)){
      wetLCC <-  reproducible::Cache(prepInputs, url = "https://drive.google.com/open?id=1YVTcIexNk-obATw2ahrgxA6uvIlr-6xm",
                                     targetFile = "wetlandsNWT250m.tif",
                                     studyArea=sim$studyArea)
      wetLCC <-  reproducible::Cache(postProcess,
                                     x = wetLCC, rasterToMatch = sim$rasterToMatch,
                                     maskWithRTM = TRUE, destinationPath = tempdir(),
                                     filename2 = NULL)
      
    }
    
    waterVals <- raster::getValues(wetLCC) # Uplands = 3, Water = 1, Wetlands = 2, so 2 and 3 to NA
    waterVals[waterVals == 1] <- 1
    waterVals[waterVals > 1] <- NA
    sim$waterRaster <- raster::setValues(wetLCC, waterVals)
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
