\name{surv_risk_aut_xgbcv}
\alias{surv_risk_aut_xgbcv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get risk level for survival data with xgbcv object
}
\description{
\code{surv_risk_m} is a function to get risk level for survival data using xgbcv object
}
\usage{
surv_risk_aut_xgbcv(
model,
x_train,
x_test
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
    model to be applied
}
  \item{x_train}{
    Train data
}
  \item{x_test}{
    Test data
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
risk level for test data
}

