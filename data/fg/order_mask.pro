pro order_mask, ord_mask


; G1xG2 5-8 microns

orders=[15, 16, 17, 18, 19, 20, 21, 22]
num_orders = n_elements(orders)

ord_mask = fltarr(256,256)

for i=0,num_orders-1 do begin      ; for each spectral order

   map=[[[194,239],[156,202],[124,165],[93,135],[68,106],[42,81],[23,56],[3,36]],$
          [[0,234]  ,[0,255]  ,[0,255]  ,[0,255] ,[0,255] ,[0,255],[0,255],[0,255]]]

    ord_height=[15,15,15,15,15,15,15,15]


    slope= float(map[1,i,0]-map[0,i,0])/float(map[1,i,1]-map[0,i,1])

    xvalue=findgen(map[1,i,1]-map[0,i,1]+1)
    
    ;yvalue, the spatial direction positions of the order in the array,
    ; calculated from the order slope
    
    yvalue=slope*xvalue+map(0,i,0)
    
    dy = (ord_height)[i]
    
    for j=0,ord_height[i]-1 do begin

        ord_mask(xvalue,yvalue+j) = 1.0

    endfor
    ;ord_mask(where(ord_mask(xvalue,yvalue:yvalue+dy))) = 0.0
 
endfor
ord_mask = rot(ord_mask,90.0)
atv22,ord_mask

end
