#Some code modified from https://bbc.github.io/rcookbook/#how_to_create_bbc_style_graphics
if(!require(pacman))install.packages("pacman")
pacman::p_load(ggplot2,sysfonts,viridis,scales,showtext)
showtext_auto()
#GP Palette
gpPal=c(
  "#2c83c3", #1 Hydrogen Blue
  "#6c2d82", #2 Lightning Purple
  "#ff3dac", #3 Flare Fucsia
  "#6812d1", #4 Atomic Blue
  "#cb1f8e", #5 Burst Purple
  "#090816", #6 Galactic Black
  "#f0f4ff" #7 Sparkle White
)
#expanded palette
gpPal2=c(
  "#2c83c3", #1 Hydrogen Blue
  "#69b2f6",   #1a light blue
  "#005792",   #1b dark blue
  "#C36C2C",  #1c complementary orange
  "#6c2d82", #2 Lightning Purple
  "#9c5ab2",    #2a light Purple
  "#3e0055",   #2b dark purple
  "#43822D",   #2c complementary dark green
  "#ff3dac", #3 Flare Fucsia
  "#ff78de",   #3a light fucsia
  "#c7007d",   #3b dark fuscia
  "#3DFF90",   #3c complementary bright green
  "#6812d1", #4 Atomic Blue
  "#9f4bff",   #4a light atomic blue
  "#29009e",   #4b dark atomic blue
  "#7BD112",   #4c complementary neon green
  "#cb1f8e", #5 Burst Purple
  "#ff5dbe",   #5a light burst purple
  "#960060",   #5b dark burst purple
  "#cb1f8e",   #5c complementary green
  "#090816", #6 Galactic Black
  "#f0f4ff" #7 Sparkle White
  
  )

#Show the GP palette
# x is the first n colors to show or x1:x2 or c(1,3,5) for a specific subset
# pal is 1 by default; if 2, shows extended palette
show.gpPal<-function(pal=1,x){
  P <- if(pal==1){gpPal}else{gpPal2}
  if(missing(x)){x=length(P)}
  s <- if(length(x)==1){eval(quote(1:x))}else{eval(quote(x))}
  scales::show_col(P[s])}

#These are default shapes
gpShps<-c(21,24,22,23,8,13,9)
plot(0:length(gpShps),0:length(gpShps),pch=gpShps,cex=2,xlim=c(-1,length(gpShps)+1),ylim=c(-1,length(gpShps)+1))
  
font="Montserrat"
  fam=font
  font_add_google(name=font,family=fam)

ggGalactic<-function(){
  font="Montserrat"
theme_linedraw()+theme(
    text=element_text(family=font),
    plot.title=element_text(family=font,size=30,face="bold",color=gpPal[6]),
    plot.subtitle=element_text(family=font,size=22,color=gpPal[5]),
    axis.title=element_text(family=font,size=28,face="bold",color=gpPal[6]),
    axis.text=element_text(family=font,size=18,color=gpPal[6]),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 20, r = 10, b = 0, l = 0)),
    legend.text=element_text(family=font,color=gpPal[6],size=18),
    legend.title=element_text(family=font,color=gpPal[6],face="bold",size=18),
    legend.position = "right", legend.text.align = 0, legend.background =element_blank()
  )
}
