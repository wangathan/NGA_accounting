##################################################
#
#   Assemble intersects into single csv and make plot of NGA density
#   over ABoVE domain
#
#

## Jon Wang
## Feb 28 2017

require('data.table')
require('ggplot2')
require("sp")
require('rgdal')
require("rgeos")
require('foreach')
require('wesanderson')
require('raster')

# to plot world/country lines
us = getData("GADM", country="USA", level=1)
can = getData("GADM", country="CAN", level=1)

usf = fortify(us)
canf = fortify(can)

ABoVE_grid = readOGR(dsn = "../../data/ABoVE/ABoVE_Grid_240m_and_30m",
                     layer = "ABoVE_30mgrid_tiles_Final")

intersectDTs = list.files("../../data/ABoVE/intersectDT", 
                          full.names=T,
                          pattern="csv")

usf_sp = SpatialPoints(usf[,c("long", "lat")], proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
usf_ab = spTransform(usf_sp, CRS(projection(ABoVE_grid)))
usf_ab_pts = cbind.data.frame(usf_ab@coords, usf$group)
colnames(usf_ab_pts) = c("long", "lat", "group")

canf_sp = SpatialPoints(canf[,c("long", "lat")], proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
canf_ab = spTransform(canf_sp, CRS(projection(ABoVE_grid)))
canf_ab_pts = cbind.data.frame(canf_ab@coords, canf$group)
colnames(canf_ab_pts) = c("long", "lat", "group")

# read in all data
intersects = foreach(f = 1:length(intersectDTs), .combine = function(x,y)rbindlist(list(x,y),use.names=T)) %do% {
  inDT = fread(intersectDTs[f])
}

ABoVE_dt = as.data.table(ABoVE_grid@data)
setkey(ABoVE_dt, UID)

# clean up and summarize
intersects[,c("UID_hits","UID_early", "UID_late", "UID_gs", "UID_hits_w", "UID_gs_w"):=

            .(sum(hits), min(early),  max(late),  sum(gs), sum(weightedHits), sum(weightedGs)),
           by=.(UID)]

intersects = unique(intersects[, .(Id, UID, Ahh, Avv, Bh, Bv, UID_hits, UID_early, UID_late, UID_gs, UID_hits_w, UID_gs_w)])
setkey(intersects, UID)

# merge in to original data to include 0s
ABoVE_int_dt = merge(ABoVE_dt, intersects, by = c("UID", "Id", "Ahh", "Avv", "Bh", "Bv"), all=T)

#ABoVE_int_dt[is.na(ABoVE_int_dt)] = 0

# merge NGA summary data into ABoVE shapefile
row.names(ABoVE_grid) = row.names(ABoVE_int_dt)
ABoVE_grid_merged = merge(ABoVE_grid, ABoVE_int_dt, by = "UID")

ABoVE_fort = fortify(ABoVE_grid_merged)
ABoVE_fort = merge(ABoVE_fort, ABoVE_int_dt, by.x="id",by.y="UID")


#ABoVE_NGA_plot_hits = ggplot() +
#  geom_polygon(data = wmap, aes(long, lat, group=group), fill=NA, color="white") +
#  coord_equal()

# make sure to restrict area, since ggplot will plot everything
usf_ab_pts[usf_ab_pts$lat > max(ABoVE_fort$lat) |
            usf_ab_pts$lat < min(ABoVE_fort$lat) |
            usf_ab_pts$long > max(ABoVE_fort$long) |
            usf_ab_pts$long < min(ABoVE_fort$long),"lat"] = NA
usf_ab_pts = na.omit(usf_ab_pts)

canf_ab_pts[canf_ab_pts$lat > max(ABoVE_fort$lat) |
            canf_ab_pts$lat < min(ABoVE_fort$lat) |
            canf_ab_pts$long > max(ABoVE_fort$long) |
            canf_ab_pts$long < min(ABoVE_fort$long),"lat"] = NA
canf_ab_pts = na.omit(canf_ab_pts)

ABoVE_NGA_plot_wgs = ggplot() +
  geom_polygon(data = ABoVE_fort, aes(long, lat, group=group, fill=UID_gs)) +
  geom_polygon(data = usf_ab_pts, aes(long, lat, group=group), fill=NA, color="white") +
  geom_polygon(data = canf_ab_pts, aes(long, lat, group=group), fill=NA, color="white") +
  geom_polygon(data = ABoVE_fort, aes(long, lat, group=group), fill=NA, color="black", size=0.15) +
  coord_equal() +
  scale_fill_gradientn("Number of\nJJAS looks", colors = wes_palette("Zissou", type="continuous")) +
  labs(x = "", y = "") +
  theme(axis.text = element_blank()) +
  ggtitle("Number of NGA looks per ABoVE tile, growing season only\nweighted by intersection area")

ABoVE_NGA_plot_gs = ggplot() +
  geom_polygon(data = ABoVE_fort, aes(long, lat, group=group, fill=UID_gs)) +
  geom_polygon(data = usf_ab_pts, aes(long, lat, group=group), fill=NA, color="white") +
  geom_polygon(data = canf_ab_pts, aes(long, lat, group=group), fill=NA, color="white") +
  geom_polygon(data = ABoVE_fort, aes(long, lat, group=group), fill=NA, color="black", size=0.15) +
  coord_equal() +
  scale_fill_gradientn("Number of\nJJAS looks", colors = wes_palette("Zissou", type="continuous")) +
  labs(x = "", y = "") +
  theme(axis.text = element_blank()) +
  ggtitle("Number of NGA looks per ABoVE tile, growing season only")

ABoVE_NGA_plot_early = ggplot() +
  geom_polygon(data = ABoVE_fort, aes(long, lat, group=group, fill=UID_early)) +
  geom_polygon(data = usf_ab_pts, aes(long, lat, group=group), fill=NA, color="white") +
  geom_polygon(data = canf_ab_pts, aes(long, lat, group=group), fill=NA, color="white") +
  geom_polygon(data = ABoVE_fort, aes(long, lat, group=group), fill=NA, color="black", size=0.15) +
  coord_equal() +
  scale_fill_gradientn("Earliest Year", colors = wes_palette("Zissou", type="continuous")) +
  labs(x = "", y = "") +
  theme(axis.text = element_blank()) +
  ggtitle("Year of earliest NGA look per ABoVE tile")

ABoVE_NGA_plot_late = ggplot() +
  geom_polygon(data = ABoVE_fort, aes(long, lat, group=group, fill=UID_late)) +
  geom_polygon(data = usf_ab_pts, aes(long, lat, group=group), fill=NA, color="white") +
  geom_polygon(data = canf_ab_pts, aes(long, lat, group=group), fill=NA, color="white") +
  geom_polygon(data = ABoVE_fort, aes(long, lat, group=group), fill=NA, color="black", size=0.15) +
  coord_equal() +
  scale_fill_gradientn("Latest Year", colors = wes_palette("Zissou", type="continuous")) +
  labs(x = "", y = "") +
  theme(axis.text = element_blank()) +
  ggtitle("Year of latest NGA look per ABoVE tile")

ABoVE_NGA_plot_whits = ggplot() +
  geom_polygon(data = ABoVE_fort, aes(long, lat, group=group, fill=UID_hits_w)) +
  geom_polygon(data = usf_ab_pts, aes(long, lat, group=group), fill=NA, color="white") +
  geom_polygon(data = canf_ab_pts, aes(long, lat, group=group), fill=NA, color="white") +
  geom_polygon(data = ABoVE_fort, aes(long, lat, group=group), fill=NA, color="black", size=0.15) +
  coord_equal() +
  scale_fill_gradientn("Number of\nNGA looks", colors = wes_palette("Zissou", type="continuous")) +
  labs(x = "", y = "") +
  theme(axis.text = element_blank()) +
  ggtitle("Number of NGA looks per ABoVE tile, annual\nweighted by intersection area")

ABoVE_NGA_plot_hits = ggplot() +
  geom_polygon(data = ABoVE_fort, aes(long, lat, group=group, fill=UID_hits)) +
  geom_polygon(data = usf_ab_pts, aes(long, lat, group=group), fill=NA, color="white") +
  geom_polygon(data = canf_ab_pts, aes(long, lat, group=group), fill=NA, color="white") +
  geom_polygon(data = ABoVE_fort, aes(long, lat, group=group), fill=NA, color="black", size=0.15) +
  coord_equal() +
  scale_fill_gradientn("Number of\nNGA looks", colors = wes_palette("Zissou", type="continuous")) +
  labs(x = "", y = "") +
  theme(axis.text = element_blank()) +
  ggtitle("Number of NGA looks per ABoVE tile, annual")

ggsave("../../plots/ABoVE/ABoVE_NGA/ABoVE_NGA_hits_wgt.png", ABoVE_NGA_plot_whits, width = 8, height = 6, units = "in")
ggsave("../../plots/ABoVE/ABoVE_NGA/ABoVE_NGA_hits.png", ABoVE_NGA_plot_hits, width = 8, height = 6, units = "in")
ggsave("../../plots/ABoVE/ABoVE_NGA/ABoVE_NGA_early.png", ABoVE_NGA_plot_early, width = 8, height = 6, units = "in")
ggsave("../../plots/ABoVE/ABoVE_NGA/ABoVE_NGA_late.png", ABoVE_NGA_plot_late, width = 8, height = 6, units = "in")
ggsave("../../plots/ABoVE/ABoVE_NGA/ABoVE_NGA_gs.png", ABoVE_NGA_plot_gs, width = 8, height = 6, units = "in")
ggsave("../../plots/ABoVE/ABoVE_NGA/ABoVE_NGA_gs_wgt.png", ABoVE_NGA_plot_wgs, width = 8, height = 6, units = "in")
ggsave("../../plots/ABoVE/ABoVE_NGA/ABoVE_NGA_gs.png", ABoVE_NGA_plot_gs, width = 8, height = 6, units = "in")

