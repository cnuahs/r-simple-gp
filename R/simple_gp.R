# A simplistic Gaussian Process object with a squared exponential
# covariance function.

# 2019-01-02 - Shaun L. Cloherty <s.cloherty@ieee.org>

SimpleGP <- R6::R6Class(
  "SimpleGP",
  public = list(
    l = NULL,      # length parameter
    sigmaf = NULL, # sample (co-)std. dev. (?)
    sigman = NULL, # noise std. dev.

    k = NULL,      # covariance matrix
    invk = NULL,

    initialize = function(l = 0.1, sigmaf = 1.0, sigman = 0.0 ) {
      # constructor
      self$l = l
      self$sigmaf = sigmaf
      self$sigman = sigman
    },

    train = function(x,y) {
      # train on the supplied data (x,y)
      private$x = x
      private$y = y

      self$k = outer(x,x,private$covfcn)
      self$invk = solve(self$k) # matrix inverse

      invisible(self)
    },

    predict = function(xstar) {
      # predict ystar (mean and variance) for the given xstar

      kstar = outer(xstar,private$x,private$covfcn)
      k2star = outer(xstar,xstar,private$covfcn)

      data.frame(mean = as.vector(kstar %*% self$invk %*% private$y), # mean
                 var = diag(k2star - kstar %*% self$invk %*% t(kstar))) # variance
    }
  ),

  private = list(
    x = vector("numeric"),
    y = vector("numeric"),

    covfcn = function(x,xprime) {
      # compute covariance between x and xprime

      # "squared exponential" covariance function
      self$sigmaf^2 * exp(-(x-xprime)^2 / 2 * self$l^2) +
        self$sigman^2 * (x == xprime)
    }
  )
)
