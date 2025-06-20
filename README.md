<<<<<<< HEAD
# 2D_Contour
=======
# ISO Polar Plot Visualization

ISO(광학 강도 분포) 데이터를 다양한 방식으로 시각화하는 Streamlit 기반 웹앱입니다.

## 주요 기능
- CSV 업로드 후 ISO Polar Plot, Cartesian Plot, Plotly 기반 인터랙티브 플롯, 히트맵 시각화
- 컬러맵, 컬러바 범위, 해상도 등 다양한 시각화 옵션 제공
- 데이터 통계, 미리보기, 처리된 데이터 다운로드 지원

## 설치 방법
```bash
# 1. 저장소 클론
$ git clone https://github.com/waterfirst/2D_Contour.git
$ cd 2D_Contour

# 2. 패키지 설치
$ pip install -r requirements.txt
```

## 실행 방법
```bash
$ streamlit run iso_plot.py
```

## CSV 파일 예시
| Theta | 0 | 30 | 60 | ... |
|-------|---|----|----|-----|
| 0     | 10| 12 | 11 | ... |
| 10    | 15| 16 | 14 | ... |
| ...   |...|... |... | ... |

- 첫 번째 컬럼: `Theta` (반지름 각도)
- 나머지 컬럼: 각 Phi 각도별 값(0, 10, 20, ...)

## 주요 시각화 예시 (텍스트)
- Smooth Polar Plot
- Enhanced Cartesian Plot
- Plotly Interactive Plot
- Plotly Heatmap

## 필요 패키지
- streamlit, pandas, numpy, matplotlib, plotly, scipy

## 라이선스
MIT License 
>>>>>>> 5eff00b (Add iso_plot.py, requirements.txt, and README.md for ISO Polar Plot Visualization)
