
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#   Test Package:              'Ctrl + Shift + T'
#' @param Dosya Dosyanın adı
#' @return Dosya içindeki home_team verileri
#' @export
hello <- function(Dosya) {
  if (!file.exists(Dosya)) {
    stop("File does not exist in the current directory")
  }

  veri <- read.csv(Dosya)
  sutun_isimleri <- colnames(veri)
  print(sutun_isimleri)

  secilen_sutun_numarasi <- as.numeric(readline(prompt = paste("Lütfen seçmek istediğiniz sütun numarasını girin (1 -", length(sutun_isimleri), "): ")))

  if (is.numeric(secilen_sutun_numarasi) && !is.na(secilen_sutun_numarasi) && secilen_sutun_numarasi >= 1 && secilen_sutun_numarasi <= length(sutun_isimleri)) {
    selectData <- veri[, secilen_sutun_numarasi]
  } else {
    return(NULL)  # Geçersiz sütun numarası durumunda NULL döndür
  }
}
duzelt <- function(Dosya) {
  veri <- read.csv(Dosya)
  sutun_isimleri <- colnames(veri)
  eksik_satirlar = 0

  eksik_satirlar <- which(apply(is.na(veri), 1, any))
  print("Eksik veri içeren satırlar var:")
  print(eksik_satirlar)

  secim <- as.numeric(readline(prompt = paste("Eksik satırları ortalama ile doldurmak için 1'e basınız.\nSatırları silmek için 2'ye basınız.")))
  if (is.na(secim)) {
    secim <- 1
  }

  if (length(eksik_satirlar) > 0 && secim == 1) {
    # Eksik verileri satır ortalaması ile doldur
    for (satir_indeks in eksik_satirlar) {
      eksik_sutunlar <- which(is.na(veri[satir_indeks, ]))
      for (sutun_indeks in eksik_sutunlar) {
        sutun_ortalama <- mean(veri[satir_indeks, ], na.rm = TRUE)
        veri[satir_indeks, sutun_indeks] <- round(sutun_ortalama, 3)
      }
    }
    veri <- na.omit(veri) # Eksik satırları sil

  } else if (secim == 2) {
    veri <- veri[-eksik_satirlar, ]

  }
  else {
    print("Eksik veri içeren satırlar var ancak 1'e veya 2'ye basmadığınız için işlem yapılmadı.")
    print(eksik_satirlar)
  }
}
grafik <- function(Dosya) {
  veri <- read.csv(Dosya)
  sutun_isimleri <- colnames(veri)
  veri <- na.omit(veri)

  print(sutun_isimleri)

  secilen_1 <- as.numeric(readline(prompt = paste("Lütfen seçmek istediğiniz ilk sütunun numarasını girin (1 -", length(sutun_isimleri), "): ")))
  secilen_2 <- as.numeric(readline(prompt = paste("Lütfen seçmek istediğiniz ikinci sütunun numarasını girin (1 -", length(sutun_isimleri), "): ")))

  if (is.na(secilen_1) || is.na(secilen_2)){
    secilen_1 <- 3
    secilen_2 <- 4
  }

  if ( secilen_1 < 1 || secilen_1 > length(sutun_isimleri) || secilen_2 < 1 || secilen_2 > length(sutun_isimleri)) {
    print("Hata: Geçersiz sütun numarası!")
    return(NULL)  # Geçersiz sütun numarası durumunda NULL döndür
  }

  selectData1 <- veri[, secilen_1]
  selectData2 <- veri[, secilen_2]

  # Eksik değerleri kontrol et ve uygun şekilde ele al
  eksik_satirlar <- which(is.na(selectData1) | is.na(selectData2))
  if (length(eksik_satirlar) > 0) {
    print("Uyarı: Eksik değerler veri setinde bulunmaktadır. Eksik değerler veri görselleştirmede kullanılmayacaktır.")
    selectData1 <- selectData1[-eksik_satirlar]
    selectData2 <- selectData2[-eksik_satirlar]
  }





  # Ana grafik oluşturma
  plot.new()
  layout(matrix(c(1, 2), nrow = 2, byrow = TRUE))  # İki alt grafik için düzen oluştur

  # İlk alt grafik: selectData1
  plot(selectData1, type = "l", col = "blue", main = paste("Line Plot: ", sutun_isimleri[secilen_1]), xlab = "Index", ylab = "Value")

  # İkinci alt grafik: selectData2
  plot(selectData2, type = "l", col = "red", main = paste("Line Plot: ", sutun_isimleri[secilen_2]), xlab = "Index", ylab = "Value")







  # KPI oluşturma (örneğin: seçilen sütunlar arasındaki korelasyon)
  if (length(selectData1) > 0 && length(selectData2) > 0) {
    korelasyon <- cor(selectData1, selectData2)
    kpi <- abs(korelasyon)  # Mutlak değeri alarak pozitif bir KPI elde ediyoruz
    print(paste("Seçilen sütunlar arasındaki korelasyon KPI'si:", kpi))
  } else {
    print("Uyarı: Eksik değerler nedeniyle KPI hesaplanamıyor.")
  }
}

