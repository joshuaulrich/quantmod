setOldClass('Date')
setClass("tradeLog",representation(
                    date="Date",
                    trade.id="numeric",
                    action="character",
                    underlying="character",
                    price="numeric",
                    quantity="numeric",
                    trade.value="numeric",
                    gain.loss="numeric",
                    account.value="numeric",
                    currency="character",
                    currency.symbol='character',
                    start.date="Date",
                    exch='character'
                    ), 
                    prototype = list(
                                date=Sys.Date(),
                                trade.id=0,
                                action='',
                                underlying='',
                                price=0,
                                quantity=0,
				                trade.value=0,
				                gain.loss=0,
				                account.value=10000,
                                currency='USD',
                                currency.symbol='$',
                                start.date=Sys.Date(),
                                exch=''
                    )
        )
setMethod("show","tradeLog",
         function(object)
         {
         cat(paste("Trade Log\t\t\tInitial Balance: ",object@account.value[1],
                   " ",object@currency,"\n\n",sep=''))
         if(object@trade.id > 0) { 
         # if no trades, don't show log
           tradeLog <- cbind(object@trade.id,object@action,object@underlying,
                             object@price,object@quantity,
                             object@trade.value,object@gain.loss,object@account.value)
           colnames(tradeLog) <- c("ID","Action","Underlying",
                                   "Price","Quantity",
                                   "Gain(Loss)","Value","Account.Balance")
           print(zoo(tradeLog,order.by=object@date))
           }
         })

"tradeLog" <-
function(initial.value=10000,currency="USD",currency.symbol="$",
         underlying='')
{
  start.date <- Sys.Date()
  tradeLog <- new('tradeLog',account.value=initial.value,currency=currency,
                             currency.symbol=currency.symbol,
                             underlying=underlying,
                             start.date=start.date)
  invisible(tradeLog)
}

"as.zoo.tradeLog" <- function() {}
"as.data.frame.tradeLog" <- function() {}
"as.tradeLog.tradeLog" <- function() {}
"as.matrix.tradeLog" <- function() {}

"addTrade" <-
function(x,date,action,price,quantity,
         underlying=NULL,currency=NULL,currency.symbol=NULL,
         exch=NULL)
{
   
}
"reverseTrade" <- function() {}
"adjustTrade" <- function() {}
"cancelTrade" <- function() {}

"print.tradeLog" <- function() {}
"show.tradeLog" <- function() {}
"plot.tradeLog" <- function() {}
"summary.tradeLog" <- function() {}
"periodReturn.tradeLog" <- function() {}

