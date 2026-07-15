import "./style.css";
import { Elm } from "./src/Main.elm";
import tileImage from "./src/assets/logo.png";
import houseImage from "./src/assets/haus.png";
import farmImage from "./src/assets/farm.png";
import schoolImage from "./src/assets/schule.png";
import storeImage from "./src/assets/kaufhaus.png";
import tavernImage from "./src/assets/taverne.png";
import wellImage from "./src/assets/brunnen.png";
import granaryImage from "./src/assets/getreidespeicher.png";
import bakeryImage from "./src/assets/baeckerei.png";

if (process.env.NODE_ENV === "development") {
    const ElmDebugTransform = await import("elm-debug-transformer")

    ElmDebugTransform.register({
        simple_mode: true
    })
}

const root = document.querySelector("#app div");
const appBaseUrl = `${window.location.origin}${import.meta.env.BASE_URL}`.replace(/\/$/, "");
const postgrestUrl = (import.meta.env.VITE_POSTGREST_URL || "").replace(/\/$/, "");
const postgrestSchema = import.meta.env.VITE_POSTGREST_SCHEMA || "www26_apesf_aquhs";
const app = Elm.Main.init({
    node: root,
    flags: {
        tileImage,
        buildingImages: [
            houseImage,
            farmImage,
            schoolImage,
            storeImage,
            tavernImage,
            wellImage,
            granaryImage,
            bakeryImage
        ],
        appBaseUrl,
        postgrestUrl,
        postgrestSchema
    }
});

if (app.ports.requestPostgrestToken) {
    app.ports.requestPostgrestToken.subscribe(async ({ username, password }) => {
        const controller = new AbortController();
        const timeoutId = window.setTimeout(() => controller.abort(), 15000);

        try {
            if (!postgrestUrl) {
                app.ports.postgrestTokenReceived.send({ ok: false, token: "", error: "Keine PostgREST-URL konfiguriert" });
                return;
            }

            const response = await fetch(`${postgrestUrl}/token`, {
                method: "POST",
                headers: {
                    Authorization: `Basic ${btoa(`${username}:${password}`)}`,
                    Accept: "application/json"
                },
                signal: controller.signal
            });

            const rawBody = await response.text();
            let body = null;

            if (rawBody) {
                try {
                    body = JSON.parse(rawBody);
                } catch {
                    body = rawBody;
                }
            }

            if (!response.ok) {
                const message = body && typeof body === "object" && body.message
                    ? body.message
                    : typeof body === "string" && body.trim()
                        ? body.trim()
                        : `Login fehlgeschlagen (${response.status})`;

                app.ports.postgrestTokenReceived.send({ ok: false, token: "", error: message });
                return;
            }

            const token = body && typeof body === "object" ? body.token : null;

            if (!token || typeof token !== "string") {
                app.ports.postgrestTokenReceived.send({ ok: false, token: "", error: "Kein Token von der API erhalten" });
                return;
            }

            app.ports.postgrestTokenReceived.send({ ok: true, token });
        } catch (error) {
            const message = error instanceof DOMException && error.name === "AbortError"
                ? "Token-Anfrage hat zu lange gedauert"
                : error instanceof Error
                    ? error.message
                    : "Login fehlgeschlagen";

            app.ports.postgrestTokenReceived.send({
                ok: false,
                token: "",
                error: message
            });
        } finally {
            window.clearTimeout(timeoutId);
        }
    });
}
