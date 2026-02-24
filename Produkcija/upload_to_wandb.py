import pandas as pd
import wandb

# ===== FUNKCIJA ZA LOGOVANJE =====
def log_model(name, rmse, mae, r2):
    run = wandb.init(
        project="eafc26-model-comparison",
        name=name,
        reinit=True
    )

    wandb.log({
        "model": name,
        "rmse": float(rmse),
        "mae": float(mae),
        "r2": float(r2)
    })

    run.finish()


# ===== UCITAVANJE CSV =====
# promeni ime fajla ako ti se drugacije zove
df = pd.read_csv("results.csv")

print("Učitani modeli:")
print(df)

# ===== LOGOVANJE SVIH MODELA =====
for _, row in df.iterrows():
    log_model(
        row["model"],
        row["RMSE"],
        row["MAE"],
        row["R2"]
    )

print("Svi modeli poslati u W&B ✅")