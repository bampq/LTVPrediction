library(data.table)

origin_data = read.csv("origin_data.csv")
adgroup_data = read.csv("adgroup_data.csv")
dest_data = read.csv("dest_data.csv")
LTV_booking_data = read.csv("LTV_booking_data.csv")
first_booking_data = read.csv("first_booking_data.csv")
setDT(origin_data)
setDT(dest_data)
setDT(adgroup_data)
setDT(first_booking_data)
setDT(LTV_booking_data)

#----------------------------------------------------------------------------------------------------------

#origin_data features
origin_data[ bkg_year == "yr1", booking_per_customer := count_booking/count_customer]
origin_data[ bkg_year == "yr1", free_booking_percentage := free_booking/count_booking]
origin_data[ bkg_year == "yr1", go_abroad_percentage := route_inter_booking/count_booking]
#new booking each channel
origin_data[ bkg_year == "yr1", new_free_booking_percentage := new_free_booking/new_booking]
origin_data[ bkg_year == "yr1", new_paid_booking_percentage := new_paid_booking/new_booking]
origin_data[ bkg_year == "yr1", new_booking_percentage := new_booking/count_booking]
#repeat booking each channel
origin_data[ bkg_year == "yr1", repeat_free_booking_percentage := repeat_free_booking/repeat_booking]
origin_data[ bkg_year == "yr1", repeat_paid_booking_percentage := repeat_paid_booking/repeat_booking]
origin_data[ bkg_year == "yr1", repeat_booking_percentage := repeat_booking/count_booking]
#yoy each channel
origin_data_yoy = merge(origin_data[bkg_year == "yr0", 
                                  list(origin_country,count_booking,free_booking,paid_booking)], 
                        origin_data[bkg_year == "yr1",
                                  list(origin_country,count_booking,free_booking,paid_booking)],
                        by = "origin_country")
origin_data_yoy[, free_channel_yoy := 
                  (free_booking.y - free_booking.x) / free_booking.x]
origin_data_yoy[, paid_channel_yoy := 
                  (paid_booking.y - paid_booking.x) / paid_booking.x]
origin_data_yoy[, all_channel_yoy := 
                  (count_booking.y - count_booking.x) / count_booking.x]
origin_data = merge(origin_data, origin_data_yoy[,
                    list(origin_country, free_channel_yoy, paid_channel_yoy, all_channel_yoy)],
                    by = "origin_country", all = TRUE)
#Select only numerical features
origin_features = origin_data_yoy[, list(origin_country)]
origin_features = merge(origin_features, origin_data[ bkg_year == "yr1", 
                  list(origin_country, booking_per_customer,
                  free_booking_percentage, go_abroad_percentage, new_free_booking_percentage,
                  new_paid_booking_percentage, new_booking_percentage, repeat_free_booking_percentage,
                  repeat_paid_booking_percentage, repeat_booking_percentage
                  )],
                  by = "origin_country", all.x = TRUE)
origin_features = merge(origin_features, origin_data_yoy[, list(origin_country, free_channel_yoy,
                  paid_channel_yoy, all_channel_yoy)], by = "origin_country")
origin_features[is.na(free_channel_yoy), free_channel_yoy := 0]
origin_features[is.na(paid_channel_yoy), paid_channel_yoy := 0]
origin_features[is.na(all_channel_yoy), all_channel_yoy := 0]
origin_features[is.infinite(free_channel_yoy), free_channel_yoy := 0]
origin_features[is.infinite(paid_channel_yoy), paid_channel_yoy := 0]
origin_features[is.infinite(all_channel_yoy), all_channel_yoy := 0]

#Group and adjust LTV_booking data by customers
LTV_booking_data[, adjusted_ttv := ifelse(channel_type == "Paid", booking_ttv/3, booking_ttv)]
LTV_customer_data = LTV_booking_data[month_after_first != "MXX", 
                    list(total_booking = .N, total_ttv = sum(adjusted_ttv)), by = list(customer_sk)]

#create data set
data_set = merge(first_booking_data, LTV_customer_data, by = "customer_sk", all.x = TRUE)
data_set[is.na(total_booking), total_booking := 0]
data_set[is.na(total_ttv), total_ttv := 0]
data_set[, total_ttv_per_first_ttv := total_ttv/first_booking_ttv]
data_set = merge(data_set, origin_features, by.x = "origin_country_name", by.y = "origin_country")
data_set_origin = data_set[Channel_Type == "Paid", list(total_first_booking = .N, 
                             total_3m_booking = sum(total_booking),
                             total_first_ttv = sum(first_booking_ttv), 
                             total_3m_ttv = sum(total_ttv)), by = list(origin_country_name)]
data_set_origin[, t3m_ttv_per_first_ttv := total_3m_ttv/total_first_ttv]
data_set_origin = merge(data_set_origin, origin_features, by.x = "origin_country_name", 
                        by.y = "origin_country")
#Create correlation matrix
cor_mat = round(cor(data_set_origin[, -1]), 2)
cor_mat_wieght = round(cov.wt(data_set_origin[, -1], wt = data_set_origin$total_first_booking,
                        cor = TRUE)$cor, 2)

#-----------------------------------------------------------------------------------------------------
 

#dest_data features
dest_data[ bkg_year == "yr1", booking_per_customer := count_booking/count_customer]
dest_data[ bkg_year =="yr1", free_booking_percentage := free_booking/count_booking]
dest_data[ bkg_year == "yr1", from_abroad_percentage := route_inter_booking/count_booking]
#new booking each channel
dest_data[ bkg_year == "yr1", new_free_booking_percentage := new_free_booking/new_booking]
dest_data[ bkg_year == "yr1", new_paid_booking_percentage := new_paid_booking/new_booking]
dest_data[ bkg_year == "yr1", new_booking_percentage := new_booking/count_booking]
#repeat booking each channel
dest_data[ bkg_year == "yr1", repeat_free_booking_percentage := repeat_free_booking/repeat_booking]
dest_data[ bkg_year == "yr1", repeat_paid_booking_percentage := repeat_paid_booking/repeat_booking]
dest_data[ bkg_year == "yr1", repeat_booking_percentage := repeat_booking/count_booking]




#yoy each channel
dest_data_yoy = merge(dest_data[bkg_year == "yr0", 
                                    list(dest_country,count_booking,free_booking,paid_booking)], 
                        dest_data[bkg_year == "yr1",
                                    list(dest_country,count_booking,free_booking,paid_booking)],
                        by = "dest_country")
dest_data_yoy[, free_channel_yoy := 
                  (free_booking.y - free_booking.x) / free_booking.x]
dest_data_yoy[, paid_channel_yoy := 
                  (paid_booking.y - paid_booking.x) / paid_booking.x]
dest_data_yoy[, all_channel_yoy := 
                  (count_booking.y - count_booking.x) / count_booking.x]
dest_data = merge(dest_data, dest_data_yoy[,
                                                 list(dest_country, free_channel_yoy, paid_channel_yoy, all_channel_yoy)],
                    by = "dest_country", all = TRUE)
#Select only numerical features
dest_features = dest_data_yoy[, list(dest_country)]
dest_features = merge(dest_features, dest_data[ bkg_year == "yr1", 
                                                      list(dest_country, booking_per_customer,
                                                           free_booking_percentage, from_abroad_percentage, new_free_booking_percentage,
                                                           new_paid_booking_percentage, new_booking_percentage, repeat_free_booking_percentage,
                                                           repeat_paid_booking_percentage, repeat_booking_percentage
                                                      )],
                        by = "dest_country", all.x = TRUE)
dest_features = merge(dest_features, dest_data_yoy[, list(dest_country, free_channel_yoy,
                                                                paid_channel_yoy, all_channel_yoy)], by = "dest_country")


#Group and adjust LTV_booking data by customers
LTV_booking_data[, adjusted_ttv := ifelse(channel_type == "Paid", booking_ttv/3, booking_ttv)]
LTV_customer_data = LTV_booking_data[month_after_first != "MXX", 
                                     list(total_booking = .N, total_ttv = sum(adjusted_ttv)), by = list(customer_sk)]

#create data set
data_set_2 = merge(first_booking_data, LTV_customer_data, by = "customer_sk", all.x = TRUE)
data_set_2 = merge(data_set_2, adgroup_data[, adgroup_id, dest_country], by = "adgroup_id", all.x = TRUE)
data_set_2 = merge(data_set_2, dest_features, by = "dest_country", all.x = TRUE)
data_set_2[is.na(total_booking), total_booking := 0]
data_set_2[is.na(total_ttv), total_ttv := 0]
data_set_2[is.na(free_channel_yoy), free_channel_yoy := 0]
data_set_2[is.na(paid_channel_yoy), paid_channel_yoy := 0]
data_set_2[is.na(all_channel_yoy), all_channel_yoy := 0]
data_set_2[is.infinite(free_channel_yoy), free_channel_yoy := 0]
data_set_2[is.infinite(paid_channel_yoy), paid_channel_yoy := 0]
data_set_2[is.infinite(all_channel_yoy), all_channel_yoy := 0]
data_set_2[, total_ttv_per_first_ttv := total_ttv/first_booking_ttv]
data_set_dest = data_set_2[Channel_Type == "Paid", list(total_first_booking = .N, 
                                                        total_3m_booking = sum(total_booking),
                                                        total_first_ttv = sum(first_booking_ttv), 
                                                        total_3m_ttv = sum(total_ttv)), by = list(dest_country)]
data_set_dest[, t3m_ttv_per_first_ttv := total_3m_ttv/total_first_ttv]
data_set_dest = merge(data_set_dest, dest_features, by = "dest_country")


#Create correlation matrix
cor_mat = round(cor(data_set_dest[, -1]), 2)
cor_mat_wieght = round(cov.wt(data_set_dest[, -1], wt = data_set_dest$total_first_booking,
                              cor = TRUE)$cor, 2)


#-----------------------------------------------------------------------------------------------------
#adgroup_data features
#by day
adgroup_data[, sun_booking_percentage := dow_sun_booking/total_booking*100]
adgroup_data[, mon_booking_percentage := dow_mon_booking/total_booking*100]
adgroup_data[, tue_booking_percentage := dow_tue_booking/total_booking*100]
adgroup_data[, wed_booking_percentage := dow_wed_booking/total_booking*100]
adgroup_data[, thu_booking_percentage := dow_thu_booking/total_booking*100]
adgroup_data[, fri_booking_percentage := dow_fri_booking/total_booking*100]
adgroup_data[, sat_booking_percentage := dow_sat_booking/total_booking*100]
#by device
adgroup_data[, mobile_percentage := dev_mobile_booking/total_booking*100]
adgroup_data[, computer_percentage := dev_computer_booking/total_booking*100]
#by route
adgroup_data[, inter_booking_percentage := route_inter_booking/total_booking*100]
adgroup_data[, domestic_booking_percentage := route_domestic_booking/total_booking*100]
adgroup_data[, first_ttv_per_room_night := 
               first_booking_total_ttv/first_booking_total_room_night]

#test
setDT(first_booking_data)
test_train_data = merge(first_booking_data[, list(origin_country_name, first_booking_ttv)],
                        origin_data[bkg_year == "yr1", list( origin_country,
                          booking_per_customer, free_booking_percentage, go_abroad_percentage,
                          new_free_booking_percentage, new_paid_booking_percentage, 
                          new_booking_percentage,
                          repeat_free_booking_percentage, repeat_paid_booking_percentage, 
                          repeat_booking_percentage,
                          free_channel_yoy, paid_channel_yoy, all_channel_yoy)],
                        by.x = "origin_country_name", by.y = "origin_country", all.x = TRUE)

