# Dashboard Peramalan Beban Listrik per Jam Panama

**Time Series Foundation Model vs Metode Statistik, Machine Learning, dan Deep Learning**

Dashboard interaktif (R Shiny) untuk membandingkan 13 model peramalan beban listrik nasional Panama
untuk horizon **168 jam (7 hari)** ke depan, dievaluasi pada **4 minggu uji** (rolling-origin cross-validation).

## Model fondasi yang diuji

| Model fondasi | Pengembang | Akses |
|---------------|------------|-------|
| TimeGPT | Nixtla | API |
| Chronos-2 | Amazon | Open-source |
| TimesFM 2.5 | Google | Open-source |

## Model yang dibandingkan

| Kelompok | Model |
|----------|-------|
| Patokan | Seasonal Naive |
| Statistik | MSTL, TBATS, DHR (Fourier + ARIMA) + eksogen |
| Machine learning | LightGBM + eksogen |
| Deep learning | N-HiTS + eksogen |
| Model fondasi | TimeGPT, Chronos-2, TimesFM 2.5 (masing-masing tanpa dan dengan eksogen) |
| Ensemble | MSTL + model fondasi terbaik |

**Metrik:** MAE, RMSE, MAPE, MASE (metrik utama), cakupan selang 80%/95%, Winkler score,
uji Diebold-Mariano (koreksi Holm), dan waktu komputasi.

## Isi dashboard

| Tab | Isi |
|-----|-----|
| Ringkasan | Model terbaik, peringkat MASE, kesimpulan otomatis |
| Eksplorasi Data | Tren bulanan, pola jam x hari, korelasi suhu, nilai ekstrem |
| Ramalan | Ramalan vs aktual per minggu, selang prediksi, galat |
| Akurasi & Peringkat | Peringkat per metrik, tabel lengkap, galat per jam |
| Selang Prediksi | Cakupan, Winkler score, grafik selang |
| Uji Signifikansi | Uji Diebold-Mariano interaktif |
| Cross-Validation | Sebaran dan kestabilan akurasi di 4 minggu |
| Efisiensi | Akurasi vs waktu komputasi |
| Metodologi | Penjelasan model, metrik, dan catatan |

Dashboard mendukung **mode terang dan gelap** lewat tombol di kanan atas.

## Struktur repositori

```
├── app.R              # Kode dashboard Shiny
├── manifest.json      # Daftar paket R untuk deployment (Posit Connect Cloud)
├── hasil_csv/         # 21 file CSV hasil notebook (sumber data dashboard)
├── notebook/          # Notebook Python untuk menjalankan semua model
├── .gitignore
└── README.md
```

## Menjalankan di komputer sendiri

1. Unduh atau *clone* repositori ini.
2. Buka `app.R` di RStudio, lalu klik **Run App**.
   Paket yang belum ada (shiny, bslib, plotly, DT, dplyr, tidyr, readr) akan terpasang otomatis.

## Memperbarui data

1. Jalankan `notebook/Perbandingan_Metode_Beban_Listrik_Panama.ipynb` (disarankan di Google Colab dengan GPU).
2. Unduh `hasil_csv.zip` dari notebook, lalu ganti isi folder `hasil_csv/` di repositori ini dengan file yang baru.
   Nama file tetap sama, jadi `app.R` dan `manifest.json` tidak perlu diubah.

> API key TimeGPT **tidak** disimpan di repositori. Isi `MY_API_KEY` di notebook hanya di komputer/Colab sendiri.

## Data

Beban listrik nasional Panama per jam (3 Januari 2015 - 27 Juni 2020, 48.048 observasi) beserta suhu di
Tocumen, Santiago, dan David, serta penanda hari libur dan masa sekolah.

## Catatan

* Model dengan eksogen memakai suhu **aktual** pada periode uji (setara prakiraan cuaca sempurna), sehingga akurasinya sedikit optimistis.
* Dataset ini tersedia publik, sehingga ada kemungkinan pernah dipakai dalam pelatihan sebagian model fondasi.
