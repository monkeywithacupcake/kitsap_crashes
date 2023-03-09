# citations
getSimplifiedCitations <- function(x) {
  x <- gsub('[[:punct:]]+','',x)
  case_when(
    grepl(paste(c("DUI", "INFLUENCE","4651502","4661502","DWUI"), collapse = "|"), x) ~ "DUI",
    grepl(paste(c("HIT AND RUN", "HIT & RUN","HITRUN","HIT RUN","HR","HIT RUN","H&R","HIT  RUN","4652020","SCENE"), collapse = "|"), x) ~ "HIT AND RUN",
    grepl(paste(c("HOMICIDE"), collapse = "|"), x) ~ "VEH HOMICIDE",
    grepl(paste(c("ASSULT", "ASSAULT","ASSAULT"), collapse = "|"), x) ~ "VEH ASSAULT",grepl(paste(c("RECKLESS", "RECK","ELUD"), collapse = "|"), x) ~ "RECKLESS",
    grepl(paste(c("STOP","FAIL TO STP","4661365"), collapse = "|"), x) ~ "FAIL TO STOP",
    grepl(paste(c("YIELD","FTY","YEILD","YEAILD","GRANT RIGHT","RIGHT OF WAY","YLD","YIE","DUE CARE"), collapse = "|"), x) ~ "FAIL TO YIELD",
    grepl(paste(c("FAIL TO OBAY","FAIL TO OBE","FAIL TO FOLLOW TRAFFIC SIGNAL","FAIL OBEY","FAILURE TO OBEY","TRAFFIC CONT","4661050","STEADY RED","ISLAND"), collapse = "|"), x) ~ "FAIL TO OBEY TRAFFIC SIGN/DEVICE/RED LIGHT",
    grepl(paste(c("4602050","4602060","4661673","INATT","INNAT","TEXTING","DISTRACTED","PHONE","HANDHELD","DEVICE","INTTENTION","INTATTENTIVE","INATENTION","INA TTENTIVE","IN-ATTENTION","WHILE DRIVING"), collapse = "|"), x) ~ "INATTENTION",
    grepl(paste(c("NEGLIGENT","NEGLIGIANT","NEG.","NEG DRIVING", "NED "), collapse = "|"), x) ~ "NEGLIGENT",
    grepl(paste(c("SPEED", "TOO FAST","FAST","4661400","SLOW FOR CURVE"), collapse = "|"), x) ~ "SPEED/TOO FAST",
    grepl("PASS",x) ~ "IMPROPER PASSING",
    grepl("BACK",x) ~ "IMPROPER BACKING",
    grepl(paste(c("START","FROM PARK"), collapse = "|"), x) ~ "UNSAFE START",
    grepl(paste(c("TOO CLOS","FLLW","FTC","TOOCLOS","TOO COLSE","TOO C","TO CLOS","4661145"), collapse = "|"), x) ~ "FOLLOWING TOO CLOSE",
    grepl("TURN",x) ~ "IMPROPER TURN",
    grepl(paste(c("WHEEL OFF","WHEELS OFF","WHEELS ON","WHEES OFF","4661670","SIDEWALK","SHOULDER"), collapse = "|"), x) ~ "OFF ROAD",
    grepl(paste(c("DRIVE ON RIGHT","DRIVING TO LEFT","LEFT OF","FAIL TO KEEP RIGHT","RIGHT OF","LFT CTR","CENTER","WRONG","4661100","4661140"), collapse = "|"), x) ~ "DRIVING LEFT OF CENTER/WRONG WAY/STAY LANE",
    grepl(paste(c("LANE","LABE","IMPROPER LN","LNE CHNG","UNSAFE LN"), collapse = "|"), x) ~ "IMPROPER LANE USE/CHANGE",
    grepl(paste(c("LOSS OF DEBRIS","SECURE LOAD","UNSECURED LOAD","UNSECURE LOAD","LOAD"), collapse = "|"), x) ~ "UNSECURE LOAD",
    grepl(paste(c("EQUIP", "DEF","IMPROPER","WDEF","UNSAFE"), collapse = "|"), x) ~ "DEFECTIVE",
    grepl(paste(c("RESTRAINT", "SAFETY","SEATBELT"), collapse = "|"), x) ~ "NO SEATBELT/RESTRAINT",
    grepl(paste(c("PERMIT","NVOL","NO VOL", "46200","NOVOL","LICENSE","NO VALID OPERATOR","DWL","SUSPENDED","ENDOR"), collapse = "|"), x) ~ "NO/SUSP LICENSE/ENDORSEMENT",
    grepl(paste(c("46300","INS","INU","PROOF OF IN","NSURANCE"), collapse = "|"), x) ~ "NO INSURANCE",
    grepl(paste(c("REG", "TABS"), collapse = "|"), x) ~ "NO REG/TABS",
    grepl(paste(c("TBD", "PEND","REFER","PC","INVEST","UNKNOWN"), collapse = "|"), x) ~ "PENDING/INVESTIGATION",
    grepl(paste(c("NONE","NA", "NO TICKET","NO CHARGE","NO ISSUED INFRACTION","NO INFRACTION","NO VIOLATIONS"), collapse = "|"), x) ~ "NONE",
    TRUE ~ "OTHER"
  )
}
