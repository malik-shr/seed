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
        ]
    }
});
