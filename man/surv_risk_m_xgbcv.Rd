\name{surv_risk_m_xgbcv}
\alias{surv_risk_m_xgbcv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get risk level for survival data manually
}
\description{
\code{surv_risk_m_xgbcv} is a function to get risk level for survival data with xgbcv object by
setting risklevel manually on train data
}
\usage{
surv_risk_m_xgbcv(
model,
train_data,
test_data,
upper,
lower
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
  \item{upper}{
    percentile for high risk level
}
  \item{upper}{
    percentile for low risk level
}
}

\value{
risk level for test data
}

