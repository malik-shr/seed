import "./style.css";
import { Elm } from "./src/Main.elm";
import tileImage from "./src/assets/logo.png";
import houseImage from "./haus.png";
import farmImage from "./farm.png";
import schoolImage from "./schule.png";
import storeImage from "./kaufhaus.png";
import tavernImage from "./taverne.png";
import wellImage from "./brunnen.png";
import granaryImage from "./getreidespeicher.png";
import bakeryImage from "./baeckerei.png";

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
