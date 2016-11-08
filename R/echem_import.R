#' Read Echem Data
#'
#' This function is used to import echem data.
#' @param echem_file Path to data file
#' @param cycler_type Name of the cycler
#' @return A data frame containing the echem data
#' @export
#' @examples
#' read.echem("/path/to/file.mpt","biologic")
#' read.echem("/path/to/file.xls","arbin")
#' read.echem("/path/to/file.txt","land")
read.echem <-function(echem_file,cycler_type)
{
	if(is.null(echem_file)|is.na(echem_file)|echem_file=='')
		return(NA)
    
  DL_cycler_types=c("arbin","biologic","land")
  DL_Functions=c(load.arbin,load.biologic,load.land)
  
  DL_Function=DL_Functions[[match(cycler_type,DL_cycler_types)]]
  
  DL_echem_temp=DL_Function(echem_file)
  return(DL_echem_temp)
}

#' Read Echem Data for Arbin Cycler
#'
#' This function is used to import echem data from an Arbin xls file.
#' @param DL_file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.arbin("/path/to/file.xls")
#' @keywords internal
load.arbin <-function(DL_file)
{
	load_or_install("gdata")
	
  DL_arbin=NULL
  tryCatch(
{
  DL_arbin = read.xls(DL_file,sheet=3,header=TRUE)
},
error=function(cond) {
  cat(sprintf('Error loading [%s]. Attempting fallback method...',DL_file))
  tryCatch(
{
  DL_arbin_sheets = sheetNames(DL_file)
  DL_arbin=data.frame()
  for(sn in DL_arbin_sheets) {
    if(grepl("^Channel_[[:digit:]]*-.*",sn)) {
      #I don't think this works.........
      DL_arbin = rbind(DL_arbin, read.xls(DL_file,sheet=sn,header=TRUE))
    }
  }
},
error=function(cond) {
  cat(sprintf('Error loading [%s]. Fallback method failed.',DL_file))
  DL_arbin=NULL
})
})

return(DL_arbin)
}

#' Read Echem Data for land (lahne) Cycler
#'
#' This function is used to import echem data from a land txt file.
#' @param DL_file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.land("/path/to/file.txt")
#' @keywords internal
load.land <-function(DL_file)
{
  DL_land=read.table(DL_file,sep="\t",header=TRUE)
  names(DL_land) <- c("Data_Point","Test_Time.s.","Step_Time.s.","Voltage.V.","Current.A.","Capacity.Ah.","Energy.Wh.","Date_Time")
  DL_land$Current.A.=DL_land$Current.A./1000
  DL_land$Capacity.Ah.=DL_land$Capacity.Ah./1000
  DL_land$Energy.Wh.=DL_land$Energy.Wh./1000
  DL_land$Step_Index = lapply(DL_land$Current.A.,function(row) {if(row==0) return(1) else if(row<0) return(2) else return(3)}) 
  ci=1
  si=1
  DL_land$Cycle_Index= lapply(DL_land$Step_Index,function(row) {if((row==1||row==2) && si==3) {ci<<-ci+1} ; si<<-row; return(ci)}) 
  DL_land$Discharge_Capacity.Ah.= mapply(function(s,c) {if(s==2) return(c) else return(0)},DL_land$Step_Index,DL_land$Capacity.Ah.)
  DL_land$Charge_Capacity.Ah.= mapply(function(s,c) {if(s==3) return(c) else return(0)},DL_land$Step_Index,DL_land$Capacity.Ah.)
  DL_land$Discharge_Energy.Wh.= mapply(function(s,c) {if(s==2) return(c) else return(0)},DL_land$Step_Index,DL_land$Energy.Wh.)
  DL_land$Charge_Energy.Wh.= mapply(function(s,c) {if(s==3) return(c) else return(0)},DL_land$Step_Index,DL_land$Energy.Wh.)
  return(DL_land)
}

#' Implementation of switch function in R
#'
#' This function is used internally to ease the importing of biologic data from different versions of ec-lab.
#' @param EXPR The vector to apply the changes to
#' @param ... The changes to apply
#' @return A vector with the changes applied
#' @examples
#' vswitch(input, changes ...)
#' @keywords internal
vswitch <- function(EXPR, ...) {
  vars <- cbind(...)
  vars[cbind(seq_along(EXPR), match(EXPR, names(list(...))))]
}

#' Read Echem Data for Biologic Cycler
#'
#' This function is used to import echem data from a biologic mpt file.
#' @param DL_file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.biologic("/path/to/file.mpt")
#' @keywords internal
load.biologic <-function(DL_file)
{
  DL_SKIP=as.integer(substr(readLines(DL_file,n=2)[[2]],18,21))
  DL_HEADER=read.table(sep="\t",header=T,text=readLines(DL_file,n=DL_SKIP+1)[c(DL_SKIP,DL_SKIP+1)])
  DL_Bio=read.table(DL_file,sep="\t",skip=DL_SKIP,header=FALSE)
  
  #Rename columns
  for(i in 1:length(names(DL_HEADER))) {
    n=names(DL_HEADER)[[i]]
    names(DL_Bio)[[i]]<-vswitch(n,
                                mode="mode",
                                ox.red="ox_red",
                                error="error",
                                control.changes="Control_changes",
                                Ns.changes="Ns_changes",
                                counter.inc.="counter inc",
                                Ns="Step_Index",
                                time.s="Test_Time.s.",
                                control.V.mA="control_v_ma",
                                Ewe.V="Voltage.V",
                                X.Q.Qo..mA.h="q-q0_mah",
                                Analog.IN.1.V="analog_in_1_v",
                                P.W="p_w",
                                Q.charge.discharge.mA.h="q_charge_discharge_mah",
                                half.cycle="half_cycle",
                                Capacitance.charge..b5.F="capacitance_charge_uf",
                                Capacitance.discharge..b5.F="capacitance_discharge_uf",
                                X.I..mA="Current.A.",
                                dq.mA.h="dq_mah",
                                Q.discharge.mA.h="Discharge_Capacity.Ah.",
                                Q.charge.mA.h="Charge_Capacity.Ah.",
                                Capacity.mA.h="capacity.mah",
                                Efficiency..="efficiency.percent",
                                control.V="control_v",
                                control.mA="control_ma",
                                cycle.number="Cycle_Index",
                                x="x")
  }
  #Adjust units etc.
  DL_Bio$Step_Index=DL_Bio$Step_Index+1
  DL_Bio$Cycle_Index=DL_Bio$Cycle_Index+1
  DL_Bio$Current.A.=DL_Bio$Current.A./1000
  DL_Bio$Discharge_Capacity.Ah.=DL_Bio$Discharge_Capacity.Ah./1000
  if(min(DL_Bio$Discharge_Capacity.Ah.)<0 && max(DL_Bio$Discharge_Capacity.Ah.)<=0) DL_Bio$Discharge_Capacity.Ah.=DL_Bio$Discharge_Capacity.Ah.*-1
  DL_Bio$Charge_Capacity.Ah.=DL_Bio$Charge_Capacity.Ah./1000
  if(min(DL_Bio$Discharge_Capacity.Ah.)<0 && max(DL_Bio$Charge_Capacity.Ah.)<=0) DL_Bio$Charge_Capacity.Ah.=DL_Bio$Discharge_Capacity.Ah.*-1
  return(DL_Bio)
}

#' Produce dummy echem data
#'
#' This function is used to produce dummy echem data.
#' @return A data frame containing the dummy echem data
#' @export
#' @examples
#' dummy.echem()
dummy.echem <-function() {
  return(data.frame(Voltage.V=c(0,0),Test_Time.s.=c(0,1)))
}

#' Combine echem experiments
#'
#' This function is used to combine two echem experiments, adjusting the time of the second experiment to follow the 1st.
#' @return A data frame containing the echem data
#' @export
#' @examples
#' echem.combine(echem_1,echem_2)
echem.combine <-function(echem_1,echem_2,time_offset=0) {
  echem_2$Test_Time.s.=echem_2$Test_Time.s.+as.numeric(echem_1[nrow(echem_1),'Test_Time.s.'])+time_offset
  return(merge(echem_1, echem_2,all=TRUE,sort=FALSE))
}