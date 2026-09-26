suppressPackageStartupMessages({library(ggplot2); library(cowplot); library(grid)})
rm(list=ls()); graphics.off()
OUT_DIR <- "."; INT_DIR <- "."
FILE_A <- file.path(INT_DIR,"Framework_panel_A_duration_distributions_centred.rds")
FILE_B <- file.path(INT_DIR,"fig02_panel_b_project_progression.rds")
FILE_C <- file.path(INT_DIR,"fig02_panel_c_design_additions.rds")
FILE_D <- file.path(INT_DIR,"fig02_panel_d_failure_risks.rds")
FILE_EF <- file.path(INT_DIR,"fig02_panels_e_f_future_pipeline.rds")
required_files <- c(FILE_A,FILE_B,FILE_C,FILE_D,FILE_EF)
missing_files <- required_files[!file.exists(required_files)]
if(length(missing_files)>0) stop(paste0("Missing required Figure 2 intermediate RDS file(s):\n",paste(missing_files,collapse="\n")))
SAVE_W_MM<-180; SAVE_H_MM<-230; SAVE_DPI<-600; FILE_STUB<-"fig02_foak_framework_v2"
col_reference_line <- "grey30"; col_proliferation_line <- "#006400"
panel_a<-readRDS(FILE_A); panel_b<-readRDS(FILE_B); panel_c<-readRDS(FILE_C); panel_d<-readRDS(FILE_D); panels_ef<-readRDS(FILE_EF)
fig_ab<-plot_grid(panel_a,panel_b,ncol=2,align="hv",axis="tblr",rel_widths=c(1,1.18))
fig_cd<-plot_grid(panel_c,panel_d,ncol=2,align="hv",axis="tblr",rel_widths=c(1,1.18))
fig_abcdef<-plot_grid(fig_ab,fig_cd,panels_ef,ncol=1,align="v",axis="lr",rel_heights=c(72,72,86))
scenario_df<-data.frame(x=c(1,2),xend=c(2,3),y=c(1,1),yend=c(1,1),lab=factor(c("Reference","Proliferation"),levels=c("Reference","Proliferation")))
scenario_legend_plot<-ggplot(scenario_df,aes(x=x,xend=xend,y=y,yend=yend,colour=lab))+geom_segment(linewidth=1)+scale_colour_manual(name="Scenario",values=c("Reference"=col_reference_line,"Proliferation"=col_proliferation_line),breaks=c("Reference","Proliferation"))+guides(colour=guide_legend(nrow=1,byrow=TRUE,override.aes=list(linewidth=1.2)))+theme_void(base_size=7)+theme(legend.position="bottom",legend.direction="horizontal",legend.title=element_text(size=6.5,face="plain",colour="black"),legend.text=element_text(size=6,colour="black"),legend.key.width=unit(8,"mm"),legend.key.height=unit(3.5,"mm"),legend.spacing.x=unit(2,"mm"),legend.box.spacing=unit(0,"mm"),legend.margin=margin(0,0,0,0))
legend_scenario<-cowplot::get_legend(scenario_legend_plot)
fig_final<-plot_grid(fig_abcdef,legend_scenario,ncol=1,rel_heights=c(1,0.035)); print(fig_final)
ggsave(file.path(OUT_DIR,paste0(FILE_STUB,".png")),fig_final,width=SAVE_W_MM,height=SAVE_H_MM,units="mm",dpi=SAVE_DPI,bg="white",limitsize=FALSE)
ggsave(file.path(OUT_DIR,paste0(FILE_STUB,".pdf")),fig_final,width=SAVE_W_MM,height=SAVE_H_MM,units="mm",bg="white",limitsize=FALSE)
saveRDS(fig_final,file.path(OUT_DIR,paste0(FILE_STUB,".rds")))
