function lorentz, x, x_c, gamma

; gamma = half-width at half-maximum

y = fltarr(n_elements(x))

for i = 0, n_elements(x)-1 DO BEGIN
   y[i] = (gamma/( (x[i]-x_c)^2.0 + gamma^2.0))/3.14159
ENDFOR

return, y

END
