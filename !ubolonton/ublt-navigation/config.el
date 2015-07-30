(defun ublt-navigation/exchange-point-and-mark ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (let ((active (region-active-p)))
    (exchange-point-and-mark)
    (when (not active)
      (deactivate-mark nil))))

(define-key global-map
  [remap exchange-point-and-mark] 'ublt-navigation/exchange-point-and-mark)
